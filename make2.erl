%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2013. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%% Purpose : Basic make facility

%% Compares date stamps of .erl and Object files - recompiles when
%% necessary.
%% Files to be checked are contained in a file 'Emakefile' 
%% If Emakefile is missing the current directory is used.
-module(make2).

-export([all/0,all/1,all/2,files/1,files/2]).
-export([format_error/1]).

-export([color_qry/1]).

-include_lib("kernel/include/file.hrl").

-import(lists, [member/2,reverse/1,reverse/2,keyfind/3,last/1,
		map/2,flatmap/2,foreach/2,foldr/3,any/2]).

-define(MakeOpts,[noexec,load,netload,noload]).


-define(red(X),     color("\e[1;31m",X,"\e[39;22m")).
-define(green(X),   color("\e[1;32m",X,"\e[39;22m")).
-define(yellow(X),  color("\e[1;33m",X,"\e[39;22m")).
-define(blue(X),    color("\e[1;34m",X,"\e[39;22m")).
-define(magenta(X), color("\e[1;35m",X,"\e[39;22m")).
-define(cyan(X),    color("\e[1;36m",X,"\e[39;22m")).
-define(grey(X),    color("\e[1;90m",X,"\e[39;22m")).

-define(recompile_prefix(), ?blue("~nCompiling:")).
-define(warning_prefix(), "==> " ++ ?yellow("[WARNING]")).
-define(error_prefix(), "==> " ++ ?red("[ERROR] ")).
-define(ok_prefix(), "==> " ++ ?green("[OK]")).
-define(srcfile(X), ?grey(X)).
-define(cached_prefix(), ?cyan("~nCached:")).

color(Prefix, X, Postfix) ->
	case call_msg(color_qry_, color) of
		true -> Prefix ++ X ++ Postfix;
		_ -> X
	end.

all() ->
    all([], undefined).

all(Options) ->
	all(Options, undefined).

all(Options, Emake) ->
    {MakeOpts,CompileOpts} = sort_options(Options,[],[]),
    put(force_compile, lists:member(force_compile, CompileOpts)),
    put(shadow_cached, lists:member(shadow_cached, CompileOpts)),
    put(less_output, lists:member(less_output, CompileOpts)),
	all(MakeOpts, CompileOpts, Emake).

all(MakeOpts, CompileOpts, undefined) ->
	Emake = read_emakefile('Emakefile'),
	all(MakeOpts, CompileOpts, Emake);
all(_MakeOpts, _CompileOpts, error) ->
	error;
all(MakeOpts, CompileOpts, enoent) ->
    %% No Emakefile found - return all modules in current 
    %% directory and the options given at command line
    Mods = [filename:rootname(F) ||  F <- filelib:wildcard("*.erl")],
    do_make_files([{Mods, CompileOpts}], MakeOpts);
all(MakeOpts, CompileOpts, Emake) when is_list(Emake) ->
    Files = transform(Emake, CompileOpts, [], []),
    do_make_files(Files, MakeOpts).

files(Fs) ->
    files(Fs, []).

files(Fs0, Options) ->
    Fs = [filename:rootname(F,".erl") || F <- Fs0],
    {MakeOpts,CompileOpts} = sort_options(Options,[],[]),
    case get_opts_from_emakefile(Fs,'Emakefile',CompileOpts) of
	Files when is_list(Files) ->
	    do_make_files(Files,MakeOpts);	    
	error -> error
    end.

do_make_files(Fs, Opts) ->
	Noshell = proplists:get_value(noshell, init:get_arguments()) =/= undefined,
	Color = case Noshell of
		true ->
			true;
		false ->
			Ver = erlang:system_info(otp_release),
			string:sub_string(Ver, 2, 3) > "15"
	end,
	%% wait pid ?
	spawn_link(fun error_summary/0),
	spawn_link(?MODULE, color_qry, [Color]),
	waitpid(2),
    process(Fs, lists:member(noexec, Opts), load_opt(Opts)),
    {W, E, C, F} = call_msg(error_summary_, stat),
    Perfect = case W+E+C of
    	0 -> ?green("(^_^)");
    	_ -> ""
    end,
	Summary = "~n   "++ Perfect
			++" ~p "
			++?yellow("Warnings")
			++", ~p "
			++?red("Errors ")
			++?magenta("in ")
			++"~p "
			++?magenta("files")
			++", "
			++?magenta("and ")
			++"~p "
			++?magenta("cached files")
			++".~n~n",
    io:format(Summary, [W, E, F-C, C]).

cast_msg(Pid, Msg) when is_atom(Pid) ->
	cast_msg(erlang:whereis(Pid), Msg);
cast_msg(Pid, Msg) ->
	Pid ! {self(), Msg}.
call_msg(Pid, Msg) when is_atom(Pid) ->
	call_msg(erlang:whereis(Pid), Msg);
call_msg(Pid, Msg) ->
	Pid ! {self(), Msg},
	receive
		{Pid, Data} ->
			Data
	end.

compile_count(Data) ->
	cast_msg(error_summary_, {data, Data}).

cached_count(File) ->
	cast_msg(error_summary_, {cached, File}).	

error_summary() ->
	erlang:register(error_summary_, self()),
	{links, [P]} = erlang:process_info(self(), links),
	P ! {ok, error_summary},
	error_summary_([]).
error_summary_(Memo) ->
	receive
		{_, {data, Data}} ->
			error_summary_([Data | Memo]);
		{_, {cached, File}} ->
			error_summary_([File | Memo]);
		{From, stat} ->
			R = lists:foldl(
				fun({_F, W, E}, {Ws, Es, Cs, Fs}) ->
					{Ws+W, Es+E, Cs, Fs+1};
				   (_, {Ws, Es, Cs, Fs}) ->
				    {Ws, Es, Cs+1, Fs+1}
			end, {0, 0, 0, 0}, Memo),
			cast_msg(From, R);
		stop ->
			ok
	end.

color_qry(Color) ->
	erlang:register(color_qry_, self()),
	{links, [P]} = erlang:process_info(self(), links),
	P ! {ok, color_qry},
	color_qry_(Color).
color_qry_(Color) ->
	receive
		{From, color} ->
			cast_msg(From, Color),
			color_qry_(Color)
	end.

waitpid(N) when N>0 ->
	receive
		{ok, color_qry} ->
			waitpid(N-1);
		{ok, error_summary} ->
			waitpid(N-1)
	end;
waitpid(_N) ->
	ok.



sort_options([H|T],Make,Comp) ->
    case lists:member(H,?MakeOpts) of
	true ->
	    sort_options(T,[H|Make],Comp);
	false ->
	    sort_options(T,Make,[H|Comp])
    end;
sort_options([],Make,Comp) ->
    {Make,lists:reverse(Comp)}.

%%% Reads the given Emakefile and returns a list of tuples: {Mods,Opts}
%%% Mods is a list of module names (strings)
%%% Opts is a list of options to be used when compiling Mods
%%%
%%% Emakefile can contain elements like this:
%%% Mod.
%%% {Mod,Opts}.
%%% Mod is a module name which might include '*' as wildcard
%%% or a list of such module names
%%%
%%% These elements are converted to [{ModList,OptList},...]
%%% ModList is a list of modulenames (strings)
read_emakefile(Emakefile) ->
    case file:consult(Emakefile) of
	{ok,Emake} ->
		Emake;
	{error,enoent} ->
		enonet;
	{error,Other} ->
	    io:format("make: Trouble reading 'Emakefile':~n~p~n",[Other]),
	    error
    end.

transform([{Mod,ModOpts}|Emake],Opts,Files,Already) ->
    case expand(Mod,Already) of
	[] -> 
	    transform(Emake,Opts,Files,Already);
	Mods -> 
	    transform(Emake,Opts,[{Mods,ModOpts++Opts}|Files],Mods++Already)
    end;
transform([Mod|Emake],Opts,Files,Already) ->
    case expand(Mod,Already) of
	[] -> 
	    transform(Emake,Opts,Files,Already);
	Mods ->
	    transform(Emake,Opts,[{Mods,Opts}|Files],Mods++Already)
    end;
transform([],_Opts,Files,_Already) ->
    lists:reverse(Files).

expand(Mod,Already) when is_atom(Mod) ->
    expand(atom_to_list(Mod),Already);
expand(Mods,Already) when is_list(Mods), not is_integer(hd(Mods)) ->
    lists:concat([expand(Mod,Already) || Mod <- Mods]);
expand(Mod,Already) ->
    case lists:member($*,Mod) of
	true -> 
	    Fun = fun(F,Acc) -> 
			  M = filename:rootname(F),
			  case lists:member(M,Already) of
			      true -> Acc;
			      false -> [M|Acc]
			  end
		  end,
	    lists:foldl(Fun, [], filelib:wildcard(Mod++".erl"));
	false ->
	    Mod2 = filename:rootname(Mod, ".erl"),
	    case lists:member(Mod2,Already) of
		true -> [];
		false -> [Mod2]
	    end
    end.

%%% Reads the given Emakefile to see if there are any specific compile 
%%% options given for the modules.
get_opts_from_emakefile(Mods,Emakefile,Opts) ->
    case file:consult(Emakefile) of
	{ok,Emake} ->
	    Modsandopts = transform(Emake,Opts,[],[]),
	    ModStrings = [coerce_2_list(M) || M <- Mods],
	    get_opts_from_emakefile2(Modsandopts,ModStrings,Opts,[]); 
	{error,enoent} ->
	    [{Mods, Opts}];
	{error,Other} ->
	    io:format("make: Trouble reading 'Emakefile':~n~p~n",[Other]),
	    error
    end.

get_opts_from_emakefile2([{MakefileMods,O}|Rest],Mods,Opts,Result) ->
    case members(Mods,MakefileMods,[],Mods) of
	{[],_} -> 
	    get_opts_from_emakefile2(Rest,Mods,Opts,Result);
	{I,RestOfMods} ->
	    get_opts_from_emakefile2(Rest,RestOfMods,Opts,[{I,O}|Result])
    end;
get_opts_from_emakefile2([],[],_Opts,Result) ->
    Result;
get_opts_from_emakefile2([],RestOfMods,Opts,Result) ->
    [{RestOfMods,Opts}|Result].
    
members([H|T],MakefileMods,I,Rest) ->
    case lists:member(H,MakefileMods) of
	true ->
	    members(T,MakefileMods,[H|I],lists:delete(H,Rest));
	false ->
	    members(T,MakefileMods,I,Rest)
    end;
members([],_MakefileMods,I,Rest) ->
    {I,Rest}.


%% Any flags that are not recognixed as make flags are passed directly
%% to the compiler.
%% So for example make:all([load,debug_info]) will make everything
%% with the debug_info flag and load it.
	
load_opt(Opts) ->
    case lists:member(netload,Opts) of
	true -> 
	    netload;
	false ->
	    case lists:member(load,Opts) of
		true ->
		    load;
		_ ->
		    noload
	    end
    end.


process([{[],_Opts}|Rest], NoExec, Load) ->
    process(Rest, NoExec, Load);
process([{[H|T],Opts}|Rest], NoExec, Load) ->
    case recompilep(coerce_2_list(H), NoExec, Load, Opts) of
	error ->
	    error;
	_ ->
	    process([{T,Opts}|Rest], NoExec, Load)
    end;
process([], _NoExec, _Load) ->
    up_to_date.

recompilep(File, NoExec, Load, Opts) ->
    ObjName = lists:append(filename:basename(File),
			   code:objfile_extension()),
    ObjFile = case lists:keysearch(outdir,1,Opts) of
		  {value,{outdir,OutDir}} ->
		      filename:join(coerce_2_list(OutDir),ObjName);
		  false ->
		      ObjName
	      end,
    case {get(force_compile), exists(ObjFile)} of
		{false, true} ->
		    recompilep1(File, NoExec, Load, Opts, ObjFile);
		_ ->
		    recompile(File, NoExec, Load, Opts)
    end.
 
recompilep1(File, NoExec, Load, Opts, ObjFile) ->
    {ok, Erl} = file:read_file_info(lists:append(File, ".erl")),
    {ok, Obj} = file:read_file_info(ObjFile),
	 recompilep1(Erl, Obj, File, NoExec, Load, Opts).

recompilep1(#file_info{mtime=Te},
	    #file_info{mtime=To}, File, NoExec, Load, Opts) when Te>To ->
    recompile(File, NoExec, Load, Opts);
recompilep1(_Erl, #file_info{mtime=To}, File, NoExec, Load, Opts) ->
    recompile2(To, File, NoExec, Load, Opts).

%% recompile2(ObjMTime, File, NoExec, Load, Opts)
%% Check if file is of a later date than include files.
recompile2(ObjMTime, File, NoExec, Load, Opts) ->
    IncludePath = include_opt(Opts),
    case check_includes(lists:append(File, ".erl"), IncludePath, ObjMTime) of
	true ->
	    recompile(File, NoExec, Load, Opts);
	false ->
		cached_count(File),
		case get(shadow_cached) of
			true ->
				ok;
			false ->
			    io:format(?cached_prefix() ++ ?srcfile("~ts\n"),[File])
		end,
	    false
    end.

include_opt([{i,Path}|Rest]) ->
    [Path|include_opt(Rest)];
include_opt([_First|Rest]) ->
    include_opt(Rest);
include_opt([]) ->
    [].

%% recompile(File, NoExec, Load, Opts)
%% Actually recompile and load the file, depending on the flags.
%% Where load can be netload | load | noload

recompile(File, true, _Load, _Opts) ->
    io:format("Out of date: ~ts\n",[File]);
recompile(File, false, noload, Opts) ->
    % io:format(?recompile_prefix() ++ ?srcfile("~ts\n"),[File]),
    file(File, [report_errors, report_warnings, error_summary |Opts]);
recompile(File, false, load, Opts) ->
    io:format("Recompile: ~ts\n",[File]),
    c:c(File, Opts);
recompile(File, false, netload, Opts) ->
    io:format("Recompile: ~ts\n",[File]),
    c:nc(File, Opts).

exists(File) ->
    case file:read_file_info(File) of
	{ok, _} ->
	    true;
	_ ->
	    false
    end.

coerce_2_list(X) when is_atom(X) ->
    atom_to_list(X);
coerce_2_list(X) ->
    X.

%%% If you an include file is found with a modification
%%% time larger than the modification time of the object
%%% file, return true. Otherwise return false.
check_includes(File, IncludePath, ObjMTime) ->
    Path = [filename:dirname(File)|IncludePath], 
    case epp:open(File, Path, []) of
	{ok, Epp} ->
	    check_includes2(Epp, File, ObjMTime);
	_Error ->
	    false
    end.
    
check_includes2(Epp, File, ObjMTime) ->
    case epp:parse_erl_form(Epp) of
	{ok, {attribute, 1, file, {File, 1}}} ->
	    check_includes2(Epp, File, ObjMTime);
	{ok, {attribute, 1, file, {IncFile, 1}}} ->
	    case file:read_file_info(IncFile) of
		{ok, #file_info{mtime=MTime}} when MTime>ObjMTime ->
		    epp:close(Epp),
		    true;
		_ ->
		    check_includes2(Epp, File, ObjMTime)
	    end;
	{ok, _} ->
	    check_includes2(Epp, File, ObjMTime);
	{eof, _} ->
	    epp:close(Epp),
	    false;
	{error, _Error} ->
	    check_includes2(Epp, File, ObjMTime)
    end.


-record(c_literal, {anno=[], val}). % val :: literal()

-record(c_module, {anno=[], name,   % name :: Tree,
           exports,     % exports :: [Tree],
           attrs,       % attrs :: [#c_def{}],
           defs}).      % defs :: [#c_def{}]


%%----------------------------------------------------------------------

-type option() :: atom() | {atom(), term()} | {'d', atom(), term()}.

%%----------------------------------------------------------------------

%%
%%  Exported functions
%%


%% file(FileName)
%% file(FileName, Options)
%%  Compile the module in file FileName.

-define(DEFAULT_OPTIONS, [verbose,report_errors,report_warnings]).

file(File, Opts) when is_list(Opts) ->
    do_compile({file,File}, Opts++env_default_opts());
file(File, Opt) ->
    file(File, [Opt|?DEFAULT_OPTIONS]).

%%
%%  Local functions
%%

-define(pass(P), {P,fun P/1}).
-define(pass(P,T), {P,fun T/1,fun P/1}).

env_default_opts() ->
    Key = "ERL_COMPILER_OPTIONS",
    case os:getenv(Key) of
	false -> [];
	Str when is_list(Str) ->
	    case erl_scan:string(Str) of
		{ok,Tokens,_} ->
		    case erl_parse:parse_term(Tokens ++ [{dot, 1}]) of
			{ok,List} when is_list(List) -> List;
			{ok,Term} -> [Term];
			{error,_Reason} ->
			    io:format("Ignoring bad term in ~s\n", [Key]),
			    []
		    end;
		{error, {_,_,_Reason}, _} ->
		    io:format("Ignoring bad term in ~s\n", [Key]),
		    []
	    end
    end.

do_compile(Input, Opts0) ->
    Opts = expand_opts(Opts0),
    Self = self(),
    Serv = spawn_link(fun() -> internal(Self, Input, Opts) end),
    receive
	{Serv,Rep} -> Rep
    end.

expand_opts(Opts0) ->
    %% {debug_info_key,Key} implies debug_info.
    Opts = case {proplists:get_value(debug_info_key, Opts0),
		 proplists:get_value(encrypt_debug_info, Opts0),
		 proplists:get_bool(debug_info, Opts0)} of
	       {undefined,undefined,_} -> Opts0;
	       {_,_,false} -> [debug_info|Opts0];
	       {_,_,_} -> Opts0
	   end,
    foldr(fun expand_opt/2, [], Opts).

expand_opt(basic_validation, Os) ->
    [no_code_generation,to_pp,binary|Os];
expand_opt(strong_validation, Os) ->
    [no_code_generation,to_kernel,binary|Os];
expand_opt(report, Os) ->
    [report_errors,report_warnings|Os];
expand_opt(return, Os) ->
    [return_errors,return_warnings|Os];
expand_opt(r12, Os) ->
    [no_recv_opt,no_line_info|Os];
expand_opt(r13, Os) ->
    [no_recv_opt,no_line_info|Os];
expand_opt(r14, Os) ->
    [no_line_info|Os];
expand_opt({debug_info_key,_}=O, Os) ->
    [encrypt_debug_info,O|Os];
expand_opt(no_float_opt, Os) ->
    %%Turn off the entire type optimization pass.
    [no_topt|Os];
expand_opt(O, Os) -> [O|Os].

%% The compile state record.
-record(compile, {filename="" :: file:filename(),
		  dir=""      :: file:filename(),
		  base=""     :: file:filename(),
		  ifile=""    :: file:filename(),
		  ofile=""    :: file:filename(),
		  module=[],
		  code=[],
		  core_code=[],
		  abstract_code=[],		%Abstract code for debugger.
		  options=[]  :: [option()],	%Options for compilation
		  mod_options=[]  :: [option()], %Options for module_info
		  errors=[],
		  warnings=[]}).

internal(Master, Input, Opts) ->
    Master ! {self(), try internal(Input, Opts)
		      catch error:Reason -> {error, Reason}
		      end}.

internal({forms,Forms}, Opts) ->
    {_,Ps} = passes(forms, Opts),
    internal_comp(Ps, "", "", #compile{code=Forms,options=Opts,
				       mod_options=Opts});
internal({file,File}, Opts) ->
    {Ext,Ps} = passes(file, Opts),
    Compile = #compile{options=Opts,mod_options=Opts},
    internal_comp(Ps, File, Ext, Compile).

internal_comp(Passes, File, Suffix, St0) ->
    Dir = filename:dirname(File),
    Base = filename:basename(File, Suffix),
    St1 = St0#compile{filename=File, dir=Dir, base=Base,
		      ifile=erlfile(Dir, Base, Suffix),
		      ofile=objfile(Base, St0)},
    Run = case member(time, St1#compile.options) of
	      true  ->
		  io:format("Compiling ~p\n", [File]),
		  fun run_tc/2;
	      false -> fun({_Name,Fun}, St) -> catch Fun(St) end
	  end,
    case fold_comp(Passes, Run, St1) of
	{ok,St2} -> comp_ret_ok(St2);
	{error,St2} -> comp_ret_err(St2)
    end.

fold_comp([{delay,Ps0}|Passes], Run, #compile{options=Opts}=St) ->
    Ps = select_passes(Ps0, Opts) ++ Passes,
    fold_comp(Ps, Run, St);
fold_comp([{Name,Test,Pass}|Ps], Run, St) ->
    case Test(St) of
	false ->				%Pass is not needed.
	    fold_comp(Ps, Run, St);
	true ->					%Run pass in the usual way.
	    fold_comp([{Name,Pass}|Ps], Run, St)
    end;
fold_comp([{Name,Pass}|Ps], Run, St0) ->
    case Run({Name,Pass}, St0) of
	{ok,St1} -> fold_comp(Ps, Run, St1);
	{error,_St1} = Error -> Error;
	{'EXIT',Reason} ->
	    Es = [{St0#compile.ifile,[{none,?MODULE,{crash,Name,Reason}}]}],
	    {error,St0#compile{errors=St0#compile.errors ++ Es}};
	Other ->
	    Es = [{St0#compile.ifile,[{none,?MODULE,{bad_return,Name,Other}}]}],
	    {error,St0#compile{errors=St0#compile.errors ++ Es}}
    end;
fold_comp([], _Run, St) -> {ok,St}.

run_tc({Name,Fun}, St) ->
    Before0 = statistics(runtime),
    Val = (catch Fun(St)),
    After0 = statistics(runtime),
    {Before_c, _} = Before0,
    {After_c, _} = After0,
    Mem0 = erts_debug:flat_size(Val)*erlang:system_info(wordsize),
    Mem = lists:flatten(io_lib:format("~.1f kB", [Mem0/1024])),
    io:format(" ~-30s: ~10.2f s ~12s\n",
	      [Name,(After_c-Before_c) / 1000,Mem]),
    Val.

comp_ret_ok(#compile{ifile=File, code=Code,warnings=Warn0,module=Mod,options=Opts}=St) ->
    case werror(St) of
        true ->
            case member(report_warnings, Opts) of
                true ->
		    io:format("~p: warnings being treated as errors\n",
			      [?MODULE]);
                false ->
		    ok
            end,
            comp_ret_err(St);
        false ->
            Warn = messages_per_file(Warn0),
		    Ok = case Warn of
	            	[] ->
	            		true;
	            	[{F, W}] ->
	            		compile_count({F, length(W), 0}),
	            		false
	            end,
	        case member(less_output, Opts) of
	        	false ->
				    io:format(?recompile_prefix() ++ ?srcfile("~ts\n"),[File]),
				    case Ok of
				    	true ->
		            		io:format(?ok_prefix()++"~n");
		            	false ->
		            		ok
		            end,
		            report_warnings(St#compile{warnings = Warn});
		        true ->
		        	ok
	        end,
            Ret1 = case member(binary, Opts) andalso
		       not member(no_code_generation, Opts) of
                       true -> [Code];
                       false -> []
                   end,
            Ret2 = case member(return_warnings, Opts) of
                       true -> Ret1 ++ [Warn];
                       false -> Ret1
                   end,
            list_to_tuple([ok,Mod|Ret2])
    end.

comp_ret_err(#compile{ifile=File,warnings=Warn0,errors=Err0,options=Opts}=St) ->
    Warn = messages_per_file(Warn0),
    Err = messages_per_file(Err0),

    Ret = case {Warn,Err} of
    	{[], []} ->
    		ok;
    	{[{F,W}], []}->
    		compile_count({F, length(W), 0}),
    		noerr;
    	{[], [{F,E}]} ->
    		compile_count({F, 0, length(E)}),
    		haserr;
    	{[{F,W}], [{F,E}]} ->
    		compile_count({F, length(W), length(E)}),
    		haserr
    end,
    LessOutput = member(less_output, Opts),
    case {Ret, LessOutput} of
    	{noerr, true} ->
    		ok;
    	{ok, true} ->
    		ok;
    	{ok, false} ->
		    io:format(?recompile_prefix() ++ ?srcfile("~ts\n"),[File]),
    		io:fomat(?ok_prefix()++"~n");
		_ ->
		    io:format(?recompile_prefix() ++ ?srcfile("~ts\n"),[File])
	end,
    report_errors(St#compile{errors=Err}),
    case LessOutput of
    	true ->
    		ok;
    	false ->
		    report_warnings(St#compile{warnings=Warn})
	end,
    case member(return_errors, Opts) of
	true -> {error,Err,Warn};
	false -> error
    end.

not_werror(St) -> not werror(St).

werror(#compile{options=Opts,warnings=Ws}) ->
    Ws =/= [] andalso member(warnings_as_errors, Opts).

%% messages_per_file([{File,[Message]}]) -> [{File,[Message]}]
messages_per_file(Ms) ->
    T = lists:sort([{File,M} || {File,Messages} <- Ms, M <- Messages]),
    PrioMs = [erl_scan, epp, erl_parse],
    {Prio0, Rest} =
        lists:mapfoldl(fun(M, A) ->
                               lists:partition(fun({_,{_,Mod,_}}) -> Mod =:= M;
                                                  (_) -> false
                                               end, A)
                       end, T, PrioMs),
    Prio = lists:sort(fun({_,{L1,_,_}}, {_,{L2,_,_}}) -> L1 =< L2 end,
                      lists:append(Prio0)),
    flatmap(fun mpf/1, [Prio, Rest]).

mpf(Ms) ->
    [{File,[M || {F,M} <- Ms, F =:= File]} ||
	File <- lists:usort([F || {F,_} <- Ms])].

%% passes(forms|file, [Option]) -> {Extension,[{Name,PassFun}]}
%%  Figure out the extension of the input file and which passes
%%  that need to be run.

passes(Type, Opts) ->
    {Ext,Passes0} = passes_1(Opts),
    Passes1 = case Type of
		  file -> Passes0;
		  forms -> tl(Passes0)
	      end,
    Passes = select_passes(Passes1, Opts),

    %% If the last pass saves the resulting binary to a file,
    %% insert a first pass to remove the file (unless the
    %% source file is a BEAM file).
    {Ext,case last(Passes) of
	     {save_binary,_TestFun,_Fun} ->
		 case Passes of
		     [{read_beam_file,_}|_] ->
			 %% The BEAM is both input and output.
			 %% Don't remove it.
			 Passes;
		     _ ->
			 [?pass(remove_file)|Passes]
		 end;
	     _ ->
		 Passes
	 end}.

passes_1([Opt|Opts]) ->
    case pass(Opt) of
	{_,_}=Res -> Res;
	none -> passes_1(Opts)
    end;
passes_1([]) ->
    {".erl",[?pass(parse_module)|standard_passes()]}.

pass(from_core) ->
    {".core",[?pass(parse_core)|core_passes()]};
pass(from_asm) ->
    {".S",[?pass(beam_consult_asm)|asm_passes()]};
pass(asm) ->
    pass(from_asm);
pass(from_beam) ->
    {".beam",[?pass(read_beam_file)|binary_passes()]};
pass(_) -> none.

%% select_passes([Command], Opts) -> [{Name,Function}]
%%  Interpret the lists of commands to return a pure list of passes.
%%
%%  Command can be one of:
%%
%%    {pass,Mod}	Will be expanded to a call to the external
%%			function Mod:module(Code, Options).  This
%%			function must transform the code and return
%%			{ok,NewCode} or {error,Term}.
%%			Example: {pass,beam_codegen}
%%
%%    {Name,Fun}	Name is an atom giving the name of the pass.
%%    			Fun is an 'fun' taking one argument: a compile record.
%%			The fun should return {ok,NewCompileRecord} or
%%			{error,NewCompileRecord}.
%%			Note: ?pass(Name) is equvivalent to {Name,fun Name/1}.
%%			Example: ?pass(parse_module)
%%
%%    {Name,Test,Fun}	Like {Name,Fun} above, but the pass will be run
%%			(and listed by the `time' option) only if Test(St)
%%			returns true.
%%
%%    {src_listing,Ext}	Produces an Erlang source listing with the
%%			the file extension Ext.  (Ext should not contain
%%			a period.)  No more passes will be run.
%%
%%    {listing,Ext}	Produce an listing of the terms in the internal
%%			representation.  The extension of the listing
%%			file will be Ext.  (Ext should not contain
%%			a period.)   No more passes will be run.
%%
%%    done              End compilation at this point.
%%
%%    {done,Ext}        End compilation at this point. Produce a listing
%%                      as with {listing,Ext}, unless 'binary' is
%%                      specified, in which case the current
%%                      representation of the code is returned without
%%                      creating an output file.
%%
%%    {iff,Flag,Cmd}	If the given Flag is given in the option list,
%%			Cmd will be interpreted as a command.
%%			Otherwise, Cmd will be ignored.
%%			Example: {iff,dcg,{listing,"codegen}}
%%
%%    {unless,Flag,Cmd}	If the given Flag is NOT given in the option list,
%%			Cmd will be interpreted as a command.
%%			Otherwise, Cmd will be ignored.
%%			Example: {unless,no_kernopt,{pass,sys_kernopt}}
%%

select_passes([{pass,Mod}|Ps], Opts) ->
    F = fun(St) ->
		case catch Mod:module(St#compile.code, St#compile.options) of
		    {ok,Code} ->
			{ok,St#compile{code=Code}};
		    {ok,Code,Ws} ->
			{ok,St#compile{code=Code,warnings=St#compile.warnings++Ws}};
		    {error,Es} ->
			{error,St#compile{errors=St#compile.errors ++ Es}}
		end
	end,
    [{Mod,F}|select_passes(Ps, Opts)];
select_passes([{src_listing,Ext}|_], _Opts) ->
    [{listing,fun (St) -> src_listing(Ext, St) end}];
select_passes([{listing,Ext}|_], _Opts) ->
    [{listing,fun (St) -> listing(Ext, St) end}];
select_passes([done|_], _Opts) ->
    [];
select_passes([{done,Ext}|_], Opts) ->
    select_passes([{unless,binary,{listing,Ext}}], Opts);
select_passes([{iff,Flag,Pass}|Ps], Opts) ->
    select_cond(Flag, true, Pass, Ps, Opts);
select_passes([{unless,Flag,Pass}|Ps], Opts) ->
    select_cond(Flag, false, Pass, Ps, Opts);
select_passes([{_,Fun}=P|Ps], Opts) when is_function(Fun) ->
    [P|select_passes(Ps, Opts)];
select_passes([{delay,Passes0}|Ps], Opts) when is_list(Passes0) ->
    %% Delay evaluation of compiler options and which compiler passes to run.
    %% Since we must know beforehand whether a listing will be produced, we
    %% will go through the list of passes and evaluate all conditions that
    %% select a list pass.
    case select_list_passes(Passes0, Opts) of
	{done,Passes} ->
	    [{delay,Passes}];
	{not_done,Passes} ->
	    [{delay,Passes}|select_passes(Ps, Opts)]
    end;
select_passes([{_,Test,Fun}=P|Ps], Opts) when is_function(Test),
					      is_function(Fun) ->
    [P|select_passes(Ps, Opts)];
select_passes([], _Opts) ->
    [];
select_passes([List|Ps], Opts) when is_list(List) ->
    case select_passes(List, Opts) of
	[] -> select_passes(Ps, Opts);
	Nested ->
	    case last(Nested) of
		{listing,_Fun} -> Nested;
		_Other         -> Nested ++ select_passes(Ps, Opts)
	    end
    end.

select_cond(Flag, ShouldBe, Pass, Ps, Opts) ->
    ShouldNotBe = not ShouldBe,
    case member(Flag, Opts) of
	ShouldBe    -> select_passes([Pass|Ps], Opts);
	ShouldNotBe -> select_passes(Ps, Opts)
    end.

%% select_list_passes([Pass], Opts) -> {done,[Pass]} | {not_done,[Pass]}
%%  Evaluate all conditions having to do with listings in the list of
%%  passes.

select_list_passes(Ps, Opts) ->
    select_list_passes_1(Ps, Opts, []).

select_list_passes_1([{iff,Flag,{listing,_}=Listing}|Ps], Opts, Acc) ->
    case member(Flag, Opts) of
	true -> {done,reverse(Acc, [Listing])};
	false -> select_list_passes_1(Ps, Opts, Acc)
    end;
select_list_passes_1([{iff,Flag,{done,Ext}}|Ps], Opts, Acc) ->
    case member(Flag, Opts) of
	false ->
	    select_list_passes_1(Ps, Opts, Acc);
	true ->
	    {done,case member(binary, Opts) of
		      false -> reverse(Acc, [{listing,Ext}]);
		      true -> reverse(Acc)
		  end}
    end;
select_list_passes_1([{iff=Op,Flag,List0}|Ps], Opts, Acc) when is_list(List0) ->
    case select_list_passes(List0, Opts) of
	{done,_}=Done -> Done;
	{not_done,List} -> select_list_passes_1(Ps, Opts, [{Op,Flag,List}|Acc])
    end;
select_list_passes_1([{unless=Op,Flag,List0}|Ps], Opts, Acc) when is_list(List0) ->
    case select_list_passes(List0, Opts) of
	{done,_}=Done -> Done;
	{not_done,List} -> select_list_passes_1(Ps, Opts, [{Op,Flag,List}|Acc])
    end;
select_list_passes_1([P|Ps], Opts, Acc) ->
    select_list_passes_1(Ps, Opts, [P|Acc]);
select_list_passes_1([], _, Acc) ->
    {not_done,reverse(Acc)}.

%% The standard passes (almost) always run.

standard_passes() ->
    [?pass(transform_module),

     {iff,makedep,[
	 ?pass(makedep),
	 {unless,binary,?pass(makedep_output)}
       ]},
     {iff,makedep,done},

     {iff,'dpp',{listing,"pp"}},
     ?pass(lint_module),
     {iff,'P',{src_listing,"P"}},
     {iff,'to_pp',{done,"P"}},

     {iff,'dabstr',{listing,"abstr"}},
     {iff,debug_info,?pass(save_abstract_code)},

     ?pass(expand_module),
     {iff,'dexp',{listing,"expand"}},
     {iff,'E',{src_listing,"E"}},
     {iff,'to_exp',{done,"E"}},

     %% Conversion to Core Erlang.
     ?pass(core_module),
     {iff,'dcore',{listing,"core"}},
     {iff,'to_core0',{done,"core"}}
     | core_passes()].

core_passes() ->
    %% Optimization and transforms of Core Erlang code.
    [{delay,
      [{unless,no_copt,
       [{core_old_inliner,fun test_old_inliner/1,fun core_old_inliner/1},
	{iff,doldinline,{listing,"oldinline"}},
	?pass(core_fold_module),
	{core_inline_module,fun test_core_inliner/1,fun core_inline_module/1},
	{iff,dinline,{listing,"inline"}},
	{core_fold_after_inline,fun test_core_inliner/1,fun core_fold_module/1},
	?pass(core_transforms)]},
       {iff,dcopt,{listing,"copt"}},
       {iff,'to_core',{done,"core"}}]}
     | kernel_passes()].

kernel_passes() ->
    %% Destructive setelement/3 optimization and core lint.
    [?pass(core_dsetel_module),
     {iff,dsetel,{listing,"dsetel"}},

     {iff,clint,?pass(core_lint_module)},
     {iff,core,?pass(save_core_code)},

     %% Kernel Erlang and code generation.
     ?pass(kernel_module),
     {iff,dkern,{listing,"kernel"}},
     {iff,'to_kernel',{done,"kernel"}},
     {pass,v3_life},
     {iff,dlife,{listing,"life"}},
     {pass,v3_codegen},
     {iff,dcg,{listing,"codegen"}}
     | asm_passes()].

asm_passes() ->
    %% Assembly level optimisations.
    [{delay,
      [{unless,no_postopt,
	[{pass,beam_block},
	 {iff,dblk,{listing,"block"}},
	 {unless,no_bopt,{pass,beam_bool}},
	 {iff,dbool,{listing,"bool"}},
	 {unless,no_topt,{pass,beam_type}},
	 {iff,dtype,{listing,"type"}},
	 {pass,beam_split},
	 {iff,dsplit,{listing,"split"}},
	 {unless,no_dead,{pass,beam_dead}},
	 {iff,ddead,{listing,"dead"}},
	 {unless,no_jopt,{pass,beam_jump}},
	 {iff,djmp,{listing,"jump"}},
	 {unless,no_peep_opt,{pass,beam_peep}},
	 {iff,dpeep,{listing,"peep"}},
	 {pass,beam_clean},
	 {iff,dclean,{listing,"clean"}},
	 {unless,no_bsm_opt,{pass,beam_bsm}},
	 {iff,dbsm,{listing,"bsm"}},
	 {unless,no_recv_opt,{pass,beam_receive}},
	 {iff,drecv,{listing,"recv"}},
	 {unless,no_stack_trimming,{pass,beam_trim}},
	 {iff,dtrim,{listing,"trim"}},
	 {pass,beam_flatten}]},

       %% If post optimizations are turned off, we still coalesce
       %% adjacent labels and remove unused labels to keep the
       %% HiPE compiler happy.
       {iff,no_postopt,
	[?pass(beam_unused_labels),
	 {pass,beam_clean}]},

       {iff,dopt,{listing,"optimize"}},
       {iff,'S',{listing,"S"}},
       {iff,'to_asm',{done,"S"}}]},
     {pass,beam_validator},
     ?pass(beam_asm)
     | binary_passes()].

binary_passes() ->
    [{native_compile,fun test_native/1,fun native_compile/1},
     {unless,binary,?pass(save_binary,not_werror)}].

%%%
%%% Compiler passes.
%%%

%% Remove the target file so we don't have an old one if the compilation fail.
remove_file(St) ->
    file:delete(St#compile.ofile),
    {ok,St}.

-record(asm_module, {module,
		     exports,
		     labels,
		     functions=[],
		     cfun,
		     code,
		     attributes=[]}).

preprocess_asm_forms(Forms) ->
    R = #asm_module{},
    R1 = collect_asm(Forms, R),
    {R1#asm_module.module,
     {R1#asm_module.module,
      R1#asm_module.exports,
      R1#asm_module.attributes,
      R1#asm_module.functions,
      R1#asm_module.labels}}.

collect_asm([], R) ->
    case R#asm_module.cfun of
	undefined ->
	    R;
	{A,B,C} ->
	    R#asm_module{functions=R#asm_module.functions++
			 [{function,A,B,C,R#asm_module.code}]}
    end;
collect_asm([{module,M} | Rest], R) ->
    collect_asm(Rest, R#asm_module{module=M});
collect_asm([{exports,M} | Rest], R) ->
    collect_asm(Rest, R#asm_module{exports=M});
collect_asm([{labels,M} | Rest], R) ->
    collect_asm(Rest, R#asm_module{labels=M});
collect_asm([{function,A,B,C} | Rest], R) ->
    R1 = case R#asm_module.cfun of
	     undefined ->
		 R;
	     {A0,B0,C0} ->
		 R#asm_module{functions=R#asm_module.functions++
			      [{function,A0,B0,C0,R#asm_module.code}]}
	 end,
    collect_asm(Rest, R1#asm_module{cfun={A,B,C}, code=[]});
collect_asm([{attributes, Attr} | Rest], R) ->
    collect_asm(Rest, R#asm_module{attributes=Attr});
collect_asm([X | Rest], R) ->
    collect_asm(Rest, R#asm_module{code=R#asm_module.code++[X]}).

beam_consult_asm(St) ->
    case file:consult(St#compile.ifile) of
	{ok, Forms0} ->
	    {Module, Forms} = preprocess_asm_forms(Forms0),
	    {ok,St#compile{module=Module, code=Forms}};
	{error,E} ->
	    Es = [{St#compile.ifile,[{none,?MODULE,{open,E}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

read_beam_file(St) ->
    case file:read_file(St#compile.ifile) of
	{ok,Beam} ->
	    Infile = St#compile.ifile,
	    case no_native_compilation(Infile, St) of
		true ->
		    {ok,St#compile{module=none,code=none}};
		false ->
		    Mod0 = filename:rootname(filename:basename(Infile)),
		    Mod = list_to_atom(Mod0),
		    {ok,St#compile{module=Mod,code=Beam,ofile=Infile}}
	    end;
	{error,E} ->
	    Es = [{St#compile.ifile,[{none,?MODULE,{open,E}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

no_native_compilation(BeamFile, #compile{options=Opts0}) ->
    case beam_lib:chunks(BeamFile, ["CInf"]) of
	{ok,{_,[{"CInf",Term0}]}} ->
	    Term = binary_to_term(Term0),

	    %% Compiler options in the beam file will override
	    %% options passed to the compiler.
	    Opts = proplists:get_value(options, Term, []) ++ Opts0,
	    member(no_new_funs, Opts) orelse not is_native_enabled(Opts);
	_ -> false
    end.

parse_module(St) ->
    Opts = St#compile.options,
    Cwd = ".",
    IncludePath = [Cwd, St#compile.dir|inc_paths(Opts)],
    R =  epp:parse_file(St#compile.ifile, IncludePath, pre_defs(Opts)),
    case R of
	{ok,Forms} ->
	    {ok,St#compile{code=Forms}};
	{error,E} ->
	    Es = [{St#compile.ifile,[{none,?MODULE,{epp,E}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

parse_core(St) ->
    case file:read_file(St#compile.ifile) of
	{ok,Bin} ->
	    case core_scan:string(binary_to_list(Bin)) of
		{ok,Toks,_} ->
		    case core_parse:parse(Toks) of
			{ok,Mod} ->
			    Name = (Mod#c_module.name)#c_literal.val,
			    {ok,St#compile{module=Name,code=Mod}};
			{error,E} ->
			    Es = [{St#compile.ifile,[E]}],
			    {error,St#compile{errors=St#compile.errors ++ Es}}
		    end;
		{error,E,_} ->
		    Es = [{St#compile.ifile,[E]}],
		    {error,St#compile{errors=St#compile.errors ++ Es}}
	    end;
	{error,E} ->
	    Es = [{St#compile.ifile,[{none,compile,{open,E}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

compile_options([{attribute,_L,compile,C}|Fs]) when is_list(C) ->
    C ++ compile_options(Fs);
compile_options([{attribute,_L,compile,C}|Fs]) ->
    [C|compile_options(Fs)];
compile_options([_F|Fs]) -> compile_options(Fs);
compile_options([]) -> [].

clean_parse_transforms(Fs) ->
    clean_parse_transforms_1(Fs, []).

clean_parse_transforms_1([{attribute,L,compile,C0}|Fs], Acc) when is_list(C0) ->
    C = lists:filter(fun({parse_transform,_}) -> false;
			(_) -> true
		     end, C0),
    clean_parse_transforms_1(Fs, [{attribute,L,compile,C}|Acc]);
clean_parse_transforms_1([{attribute,_,compile,{parse_transform,_}}|Fs], Acc) ->
    clean_parse_transforms_1(Fs, Acc);
clean_parse_transforms_1([F|Fs], Acc) ->
    clean_parse_transforms_1(Fs, [F|Acc]);
clean_parse_transforms_1([], Acc) -> reverse(Acc).

transforms(Os) -> [ M || {parse_transform,M} <- Os ].

transform_module(#compile{options=Opt,code=Code0}=St0) ->
    %% Extract compile options from code into options field.
    case transforms(Opt ++ compile_options(Code0)) of
	[] -> {ok,St0};				%No parse transforms.
	Ts ->
	    %% Remove parse_transform attributes from the abstract code to
	    %% prevent parse transforms to be run more than once.
	    Code = clean_parse_transforms(Code0),
	    St = St0#compile{code=Code},
	    foldl_transform(St, Ts)
    end.

foldl_transform(St, [T|Ts]) ->
    Name = "transform " ++ atom_to_list(T),
    Fun = fun(S) -> T:parse_transform(S#compile.code, S#compile.options) end,
    Run = case member(time, St#compile.options) of
	      true  -> fun run_tc/2;
	      false -> fun({_Name,F}, S) -> catch F(S) end
	  end,
    case Run({Name, Fun}, St) of
	{error,Es,Ws} ->
	    {error,St#compile{warnings=St#compile.warnings ++ Ws,
			      errors=St#compile.errors ++ Es}};
	{'EXIT',R} ->
	    Es = [{St#compile.ifile,[{none,compile,{parse_transform,T,R}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}};
	{warning, Forms, Ws} ->
	    foldl_transform(
	      St#compile{code=Forms,
			 warnings=St#compile.warnings ++ Ws}, Ts);
	Forms ->
	    foldl_transform(St#compile{code=Forms}, Ts)
    end;
foldl_transform(St, []) -> {ok,St}.

get_core_transforms(Opts) -> [M || {core_transform,M} <- Opts].

core_transforms(St) ->
    %% The options field holds the complete list of options at this
    Ts = get_core_transforms(St#compile.options),
    foldl_core_transforms(St, Ts).

foldl_core_transforms(St, [T|Ts]) ->
    Name = "core transform " ++ atom_to_list(T),
    Fun = fun(S) -> T:core_transform(S#compile.code, S#compile.options) end,
    Run = case member(time, St#compile.options) of
	      true  -> fun run_tc/2;
	      false -> fun({_Name,F}, S) -> catch F(S) end
	  end,
    case Run({Name, Fun}, St) of
	{'EXIT',R} ->
	    Es = [{St#compile.ifile,[{none,compile,{core_transform,T,R}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}};
	Forms ->
	    foldl_core_transforms(St#compile{code=Forms}, Ts)
    end;
foldl_core_transforms(St, []) -> {ok,St}.

%%% Fetches the module name from a list of forms. The module attribute must
%%% be present.
get_module([{attribute,_,module,{M,_As}} | _]) -> M;
get_module([{attribute,_,module,M} | _]) -> M;
get_module([_ | Rest]) ->
    get_module(Rest).

%%% A #compile state is returned, where St.base has been filled in
%%% with the module name from Forms, as a string, in case it wasn't
%%% set in St (i.e., it was "").
add_default_base(St, Forms) ->
    F = St#compile.filename,
    case F of
	"" ->
 	    M = case get_module(Forms) of
 		    PackageModule when is_list(PackageModule) -> last(PackageModule);
		    M0 -> M0
 		end,
	    St#compile{base = atom_to_list(M)};
	_ ->
	    St
    end.

lint_module(St) ->
    case erl_lint:module(St#compile.code,
			 St#compile.ifile, St#compile.options) of
	{ok,Ws} ->
	    %% Insert name of module as base name, if needed. This is
	    %% for compile:forms to work with listing files.
	    St1 = add_default_base(St, St#compile.code),
	    {ok,St1#compile{warnings=St1#compile.warnings ++ Ws}};
	{error,Es,Ws} ->
	    {error,St#compile{warnings=St#compile.warnings ++ Ws,
			      errors=St#compile.errors ++ Es}}
    end.

core_lint_module(St) ->
    case core_lint:module(St#compile.code, St#compile.options) of
	{ok,Ws} ->
	    {ok,St#compile{warnings=St#compile.warnings ++ Ws}};
	{error,Es,Ws} ->
	    {error,St#compile{warnings=St#compile.warnings ++ Ws,
			      errors=St#compile.errors ++ Es}}
    end.

makedep(#compile{code=Code,options=Opts}=St) ->
    Ifile = St#compile.ifile,
    Ofile = St#compile.ofile,

    %% Get the target of the Makefile rule.
    Target0 =
	case proplists:get_value(makedep_target, Opts) of
	    undefined ->
		%% The target is derived from the output filename: possibly
		%% remove the current working directory to obtain a relative
		%% path.
		shorten_filename(Ofile);
	    T ->
		%% The caller specified one.
		T
	end,

    %% Quote the target is the called asked for this.
    Target1 = case proplists:get_value(makedep_quote_target, Opts) of
		  true ->
		      %% For now, only "$" is replaced by "$$".
		      Fun = fun
				($$) -> "$$";
				(C)  -> C
			    end,
		      map(Fun, Target0);
		  _ ->
		      Target0
	      end,
    Target = Target1 ++ ":",

    %% List the dependencies (includes) for this target.
    {MainRule,PhonyRules} = makedep_add_headers(
      Ifile,          % The input file name.
      Code,           % The parsed source.
      [],             % The list of dependencies already added.
      length(Target), % The current line length.
      Target,         % The target.
      "",             % Phony targets.
      Opts),

    %% Prepare the content of the Makefile. For instance:
    %%   hello.erl: hello.hrl common.hrl
    %%
    %% Or if phony targets are enabled:
    %%   hello.erl: hello.hrl common.hrl
    %%
    %%   hello.hrl:
    %%
    %%   common.hrl:
    Makefile = case proplists:get_value(makedep_phony, Opts) of
		   true -> MainRule ++ PhonyRules;
		   _ -> MainRule
	       end,
    {ok,St#compile{code=iolist_to_binary([Makefile,"\n"])}}.

makedep_add_headers(Ifile, [{attribute,_,file,{File,_}}|Rest],
		    Included, LineLen, MainTarget, Phony, Opts) ->
    %% The header "File" exists, add it to the dependencies.
    {Included1,LineLen1,MainTarget1,Phony1} =
	makedep_add_header(Ifile, Included, LineLen, MainTarget, Phony, File),
    makedep_add_headers(Ifile, Rest, Included1, LineLen1,
			MainTarget1, Phony1, Opts);
makedep_add_headers(Ifile, [{error,{_,epp,{include,file,File}}}|Rest],
		    Included, LineLen, MainTarget, Phony, Opts) ->
    %% The header "File" doesn't exist, do we add it to the dependencies?
    case proplists:get_value(makedep_add_missing, Opts) of
        true ->
            {Included1,LineLen1,MainTarget1,Phony1} =
		makedep_add_header(Ifile, Included, LineLen, MainTarget,
				   Phony, File),
            makedep_add_headers(Ifile, Rest, Included1, LineLen1,
				MainTarget1, Phony1, Opts);
        _ ->
            makedep_add_headers(Ifile, Rest, Included, LineLen,
				MainTarget, Phony, Opts)
    end;
makedep_add_headers(Ifile, [_|Rest], Included, LineLen,
		    MainTarget, Phony, Opts) ->
    makedep_add_headers(Ifile, Rest, Included,
			LineLen, MainTarget, Phony, Opts);
makedep_add_headers(_Ifile, [], _Included, _LineLen,
		    MainTarget, Phony, _Opts) ->
    {MainTarget,Phony}.

makedep_add_header(Ifile, Included, LineLen, MainTarget, Phony, File) ->
    case member(File, Included) of
	true ->
	    %% This file was already listed in the dependencies, skip it.
            {Included,LineLen,MainTarget,Phony};
	false ->
            Included1 = [File|Included],

	    %% Remove "./" in front of the dependency filename.
	    File1 = case File of
			"./" ++ File0 -> File0;
			_ -> File
	    end,

	    %% Prepare the phony target name.
	    Phony1 = case File of
			 Ifile -> Phony;
			 _     -> Phony ++ "\n\n" ++ File1 ++ ":"
	    end,

	    %% Add the file to the dependencies. Lines longer than 76 columns
	    %% are splitted.
	    if
		LineLen + 1 + length(File1) > 76 ->
                    LineLen1 = 2 + length(File1),
                    MainTarget1 = MainTarget ++ " \\\n  " ++ File1,
                    {Included1,LineLen1,MainTarget1,Phony1};
		true ->
                    LineLen1 = LineLen + 1 + length(File1),
                    MainTarget1 = MainTarget ++ " " ++ File1,
                    {Included1,LineLen1,MainTarget1,Phony1}
	    end
    end.

makedep_output(#compile{code=Code,options=Opts,ofile=Ofile}=St) ->
    %% Write this Makefile (Code) to the selected output.
    %% If no output is specified, the default is to write to a file named after
    %% the output file.
    Output0 = case proplists:get_value(makedep_output, Opts) of
		  undefined ->
		      %% Prepare the default filename.
		      outfile(filename:basename(Ofile, ".beam"), "Pbeam", Opts);
		  O ->
		      O
	      end,

    %% If the caller specified an io_device(), there's nothing to do. If he
    %% specified a filename, we must create it. Furthermore, this created file
    %% must be closed before returning.
    Ret = case Output0 of
	      _ when is_list(Output0) ->
		  case file:delete(Output0) of
		      Ret2 when Ret2 =:= ok; Ret2 =:= {error,enoent} ->
			  case file:open(Output0, [write]) of
			      {ok,IODev} ->
				  {ok,IODev,true};
			      {error,Reason2} ->
				  {error,open,Reason2}
			  end;
		      {error,Reason1} ->
			  {error,delete,Reason1}
		  end;
	      _ ->
		  {ok,Output0,false}
	  end,

    case Ret of
	{ok,Output1,CloseOutput} ->
	    try
		%% Write the Makefile.
		io:fwrite(Output1, "~s", [Code]),
		%% Close the file if relevant.
		if
		    CloseOutput -> file:close(Output1);
		    true -> ok
		end,
		{ok,St}
	    catch
		exit:_ ->
		    %% Couldn't write to output Makefile.
		    Err = {St#compile.ifile,[{none,?MODULE,write_error}]},
		    {error,St#compile{errors=St#compile.errors++[Err]}}
	    end;
	{error,open,Reason} ->
	    %% Couldn't open output Makefile.
	    Err = {St#compile.ifile,[{none,?MODULE,{open,Reason}}]},
	    {error,St#compile{errors=St#compile.errors++[Err]}};
	{error,delete,Reason} ->
	    %% Couldn't open output Makefile.
	    Err = {St#compile.ifile,[{none,?MODULE,{delete,Output0,Reason}}]},
	    {error,St#compile{errors=St#compile.errors++[Err]}}
    end.

%% expand_module(State) -> State'
%%  Do the common preprocessing of the input forms.

expand_module(#compile{code=Code,options=Opts0}=St0) ->
    {Mod,Exp,Forms,Opts1} = sys_pre_expand:module(Code, Opts0),
    Opts = expand_opts(Opts1),
    {ok,St0#compile{module=Mod,options=Opts,code={Mod,Exp,Forms}}}.

core_module(#compile{code=Code0,options=Opts}=St) ->
    {ok,Code,Ws} = v3_core:module(Code0, Opts),
    {ok,St#compile{code=Code,warnings=St#compile.warnings ++ Ws}}.

core_fold_module(#compile{code=Code0,options=Opts,warnings=Warns}=St) ->
    {ok,Code,Ws} = sys_core_fold:module(Code0, Opts),
    {ok,St#compile{code=Code,warnings=Warns ++ Ws}}.

test_old_inliner(#compile{options=Opts}) ->
    %% The point of this test is to avoid loading the old inliner
    %% if we know that it will not be used.
    any(fun({inline,_}) -> true;
	   (_) -> false
	end, Opts).

test_core_inliner(#compile{options=Opts}) ->
    case any(fun(no_inline) -> true;
		(_) -> false
	     end, Opts) of
	true -> false;
	false ->
	    any(fun(inline) -> true;
		   (_) -> false
		end, Opts)
    end.

core_old_inliner(#compile{code=Code0,options=Opts}=St) ->
    {ok,Code} = sys_core_inline:module(Code0, Opts),
    {ok,St#compile{code=Code}}.

core_inline_module(#compile{code=Code0,options=Opts}=St) ->
    Code = cerl_inline:core_transform(Code0, Opts),
    {ok,St#compile{code=Code}}.

core_dsetel_module(#compile{code=Code0,options=Opts}=St) ->
    {ok,Code} = sys_core_dsetel:module(Code0, Opts),
    {ok,St#compile{code=Code}}.

kernel_module(#compile{code=Code0,options=Opts}=St) ->
    {ok,Code,Ws} = v3_kernel:module(Code0, Opts),
    {ok,St#compile{code=Code,warnings=St#compile.warnings ++ Ws}}.

save_abstract_code(#compile{ifile=File}=St) ->
    case abstract_code(St) of
	{ok,Code} ->
	    {ok,St#compile{abstract_code=Code}};
	{error,Es} ->
	    {error,St#compile{errors=St#compile.errors ++ [{File,Es}]}}
    end.

abstract_code(#compile{code=Code,options=Opts,ofile=OFile}) ->
    Abstr = erlang:term_to_binary({raw_abstract_v1,Code}, [compressed]),
    case member(encrypt_debug_info, Opts) of
	true ->
	    case keyfind(debug_info_key, 1, Opts) of
		{_,Key} ->
		    encrypt_abs_code(Abstr, Key);
		false ->
		    %% Note: #compile.module has not been set yet.
		    %% Here is an approximation that should work for
		    %% all valid cases.
		    Module = list_to_atom(filename:rootname(filename:basename(OFile))),
		    Mode = proplists:get_value(crypto_mode, Opts, des3_cbc),
		    case beam_lib:get_crypto_key({debug_info, Mode, Module, OFile}) of
			error ->
			    {error, [{none,?MODULE,no_crypto_key}]};
			Key ->
			    encrypt_abs_code(Abstr, {Mode, Key})
		    end
	    end;
	false ->
	    {ok, Abstr}
    end.

encrypt_abs_code(Abstr, Key0) ->
    try
	{Mode,RealKey} = generate_key(Key0),
	case start_crypto() of
	    ok -> {ok,encrypt(Mode, RealKey, Abstr)};
	    {error,_}=E -> E
	end
    catch
	error:_ ->
	    {error,[{none,?MODULE,bad_crypto_key}]}
    end.

start_crypto() ->
    try crypto:start() of
	{error,{already_started,crypto}} -> ok;
	ok -> ok
    catch
	error:_ ->
	    {error,[{none,?MODULE,no_crypto}]}
    end.

generate_key({Mode,String}) when is_atom(Mode), is_list(String) ->
    {Mode,beam_lib:make_crypto_key(Mode, String)};
generate_key(String) when is_list(String) ->
    generate_key({des3_cbc,String}).

encrypt(des3_cbc=Mode, {K1,K2,K3, IVec}, Bin0) ->
    Bin1 = case byte_size(Bin0) rem 8 of
	       0 -> Bin0;
	       N -> list_to_binary([Bin0,random_bytes(8-N)])
	   end,
    Bin = crypto:des3_cbc_encrypt(K1, K2, K3, IVec, Bin1),
    ModeString = atom_to_list(Mode),
    list_to_binary([0,length(ModeString),ModeString,Bin]).

random_bytes(N) ->
    {A,B,C} = now(),
    random:seed(A, B, C),
    random_bytes_1(N, []).

random_bytes_1(0, Acc) -> Acc;
random_bytes_1(N, Acc) -> random_bytes_1(N-1, [random:uniform(255)|Acc]).

save_core_code(St) ->
    {ok,St#compile{core_code=cerl:from_records(St#compile.code)}}.

beam_unused_labels(#compile{code=Code0}=St) ->
    Code = beam_jump:module_labels(Code0),
    {ok,St#compile{code=Code}}.

beam_asm(#compile{ifile=File,code=Code0,
		  abstract_code=Abst,mod_options=Opts0}=St) ->
    Source = filename:absname(File),
    Opts1 = lists:map(fun({debug_info_key,_}) -> {debug_info_key,'********'};
			 (Other) -> Other
		      end, Opts0),
    Opts2 = [O || O <- Opts1, effects_code_generation(O)],
    case beam_asm:module(Code0, Abst, Source, Opts2) of
	{ok,Code} -> {ok,St#compile{code=Code,abstract_code=[]}}
    end.

test_native(#compile{options=Opts}) ->
    %% This test is done late, in case some other option has turned off native.
    %% 'native' given on the command line can be overridden by
    %% 'no_native' in the module itself.
    is_native_enabled(Opts).

is_native_enabled([native|_]) -> true;
is_native_enabled([no_native|_]) -> false;
is_native_enabled([_|Opts]) -> is_native_enabled(Opts);
is_native_enabled([]) -> false.

native_compile(#compile{code=none}=St) -> {ok,St};
native_compile(St) ->
    case erlang:system_info(hipe_architecture) of
	undefined ->
	    Ws = [{St#compile.ifile,[{none,compile,no_native_support}]}],
	    {ok,St#compile{warnings=St#compile.warnings ++ Ws}};
	_ ->
	    native_compile_1(St)
    end.

native_compile_1(St) ->
    Opts0 = St#compile.options,
    IgnoreErrors = member(ignore_native_errors, Opts0),
    Opts = case keyfind(hipe, 1, Opts0) of
	       {hipe,L} when is_list(L) -> L;
	       {hipe,X} -> [X];
	       _ -> []
	   end,
    try hipe:compile(St#compile.module,
		     St#compile.core_code,
		     St#compile.code,
		     Opts) of
	{ok,{_Type,Bin}=T} when is_binary(Bin) ->
	    {ok,embed_native_code(St, T)};
	{error,R} ->
	    case IgnoreErrors of
		true ->
		    Ws = [{St#compile.ifile,[{?MODULE,{native,R}}]}],
		    {ok,St#compile{warnings=St#compile.warnings ++ Ws}};
		false ->
		    Es = [{St#compile.ifile,[{?MODULE,{native,R}}]}],
		    {error,St#compile{errors=St#compile.errors ++ Es}}
	    end
    catch
	Class:R ->
	    Stk = erlang:get_stacktrace(),
	    case IgnoreErrors of
		true ->
		    Ws = [{St#compile.ifile,
			   [{?MODULE,{native_crash,R,Stk}}]}],
		    {ok,St#compile{warnings=St#compile.warnings ++ Ws}};
		false ->
		    erlang:raise(Class, R, Stk)
	    end
    end.

embed_native_code(St, {Architecture,NativeCode}) ->
    {ok, _, Chunks0} = beam_lib:all_chunks(St#compile.code),
    ChunkName = hipe_unified_loader:chunk_name(Architecture),
    Chunks1 = lists:keydelete(ChunkName, 1, Chunks0),
    Chunks = Chunks1 ++ [{ChunkName,NativeCode}],
    {ok, BeamPlusNative} = beam_lib:build_module(Chunks),
    St#compile{code=BeamPlusNative}.

%% effects_code_generation(Option) -> true|false.
%%  Determine whether the option could have any effect on the
%%  generated code in the BEAM file (as opposed to how
%%  errors will be reported).

effects_code_generation(Option) ->
    case Option of 
	beam -> false;
	report_warnings -> false;
	report_errors -> false;
	return_errors-> false;
	return_warnings-> false;
	binary -> false;
	verbose -> false;
	{cwd,_} -> false;
	_ -> true
    end.

save_binary(#compile{code=none}=St) -> {ok,St};
save_binary(#compile{module=Mod,ofile=Outfile,
		     options=Opts}=St) ->
    %% Test that the module name and output file name match.
    %% We must take care to not completely break a packaged module
    %% (even though packages still is as an experimental, unsupported
    %% feature) - so we will extract the last part of a packaged
    %% module name and compare only that.
    case member(no_error_module_mismatch, Opts) of
	true ->
	    save_binary_1(St);
	false ->
	    Base = filename:rootname(filename:basename(Outfile)),
	    case lists:last(packages:split(Mod)) of
		Base ->
		    save_binary_1(St);
		_ ->
		    Es = [{St#compile.ofile,
			   [{?MODULE,{module_name,Mod,Base}}]}],
		    {error,St#compile{errors=St#compile.errors ++ Es}}
	    end
    end.

save_binary_1(St) ->
    Ofile = St#compile.ofile,
    Tfile = tmpfile(Ofile),		%Temp working file
    case write_binary(Tfile, St#compile.code, St) of
	ok ->
	    case file:rename(Tfile, Ofile) of
		ok ->
		    {ok,St};
		{error,RenameError} ->
		    Es0 = [{Ofile,[{?MODULE,{rename,Tfile,Ofile,
					     RenameError}}]}],
		    Es = case file:delete(Tfile) of
			     ok -> Es0;
			     {error,DeleteError} ->
				 Es0 ++
				     [{Ofile,
				       [{?MODULE,{delete_temp,Tfile,
						  DeleteError}}]}]
			 end,
		    {error,St#compile{errors=St#compile.errors ++ Es}}
	    end;
	{error,_Error} ->
	    Es = [{Tfile,[{compile,write_error}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

write_binary(Name, Bin, St) ->
    Opts = case member(compressed, St#compile.options) of
	       true -> [compressed];
	       false -> []
	   end,
    case file:write_file(Name, Bin, Opts) of
	ok -> ok;
	{error,_}=Error -> Error
    end.

%% report_errors(State) -> ok
%% report_warnings(State) -> ok

report_errors(#compile{options=Opts,errors=Errors}) ->
    case member(report_errors, Opts) of
	true ->
	    foreach(fun ({{F,_L},Eds}) -> list_errors(F, Eds);
			({F,Eds}) -> list_errors(F, Eds) end,
		    Errors);
	false -> ok
    end.

report_warnings(#compile{options=Opts,warnings=Ws0}) ->
    Werror = member(warnings_as_errors, Opts),
    P = case Werror of
	    true -> "";
	    false -> ?warning_prefix()
	end,

    ReportWerror = Werror andalso member(report_errors, Opts),
    case member(report_warnings, Opts) orelse ReportWerror of
		true ->
		    Ws1 = flatmap(fun({{F,_L},Eds}) -> format_message(F, P, Eds);
				     ({F,Eds}) -> format_message(F, P, Eds) end,
				  Ws0),
		    Ws = lists:sort(Ws1),
		    foreach(fun({_,Str}) -> io:put_chars(Str) end, Ws);
		false -> ok
    end.

format_message(F, P, [{{Line,Column}=Loc,Mod,E}|Es]) ->
    M = {{F,Loc},io_lib:format("~s line ~w,column ~w: ~s\n",
                                [P,Line,Column,Mod:format_error(E)])},
    [M|format_message(F, P, Es)];
format_message(F, P, [{Line,Mod,E}|Es]) ->
    M = {{F,{Line,0}},io_lib:format("~s line ~w: ~s\n",
                                [P,Line,Mod:format_error(E)])},
    [M|format_message(F, P, Es)];
format_message(F, P, [{Mod,E}|Es]) ->
    M = {none,io_lib:format("~s~s\n", [P,Mod:format_error(E)])},
    [M|format_message(F, P, Es)];
format_message(_, _, []) -> [].

list_errors(F, [{{Line,Column},Mod,E}|Es]) ->
    io:fwrite(?error_prefix()++"line ~w,column ~w: ~s\n", [Line,Column,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(F, [{Line,Mod,E}|Es]) ->
    io:fwrite(?error_prefix()++"line ~w: ~s\n", [Line,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(F, [{Mod,E}|Es]) ->
    io:fwrite(?error_prefix()++"~s\n", [Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(_F, []) -> ok.

%% format_error(ErrorDescriptor) -> string()

format_error(no_native_support) ->
    "this system is not configured for native-code compilation.";
format_error(no_crypto) ->
    "this system is not configured with crypto support.";
format_error(bad_crypto_key) ->
    "invalid crypto key.";
format_error(no_crypto_key) ->
    "no crypto key supplied.";
format_error({native, E}) ->
    io_lib:fwrite("native-code compilation failed with reason: ~P.",
		  [E, 25]);
format_error({native_crash,E,Stk}) ->
    io_lib:fwrite("native-code compilation crashed with reason: ~P.\n~P\n",
		  [E,25,Stk,25]);
format_error({open,E}) ->
    io_lib:format("open error '~s'", [file:format_error(E)]);
format_error({epp,E}) ->
    epp:format_error(E);
format_error(write_error) ->
    "error writing file";
format_error({rename,From,To,Error}) ->
    io_lib:format("failed to rename ~s to ~s: ~s",
		  [From,To,file:format_error(Error)]);
format_error({delete,File,Error}) ->
    io_lib:format("failed to delete file ~s: ~s",
		  [File,file:format_error(Error)]);
format_error({delete_temp,File,Error}) ->
    io_lib:format("failed to delete temporary file ~s: ~s",
		  [File,file:format_error(Error)]);
format_error({parse_transform,M,R}) ->
    io_lib:format("error in parse transform '~s': ~p", [M, R]);
format_error({core_transform,M,R}) ->
    io_lib:format("error in core transform '~s': ~p", [M, R]);
format_error({crash,Pass,Reason}) ->
    io_lib:format("internal error in ~p;\ncrash reason: ~p", [Pass,Reason]);
format_error({bad_return,Pass,Reason}) ->
    io_lib:format("internal error in ~p;\nbad return value: ~p", [Pass,Reason]);
format_error({module_name,Mod,Filename}) ->
    io_lib:format("Module name '~s' does not match file name '~s'",
		  [Mod,Filename]).

%% erlfile(Dir, Base) -> ErlFile
%% outfile(Base, Extension, Options) -> OutputFile
%% objfile(Base, Target, Options) -> ObjFile
%% tmpfile(ObjFile) -> TmpFile
%%  Work out the correct input and output file names.

erlfile(".", Base, Suffix) ->
    Base ++ Suffix;
erlfile(Dir, Base, Suffix) ->
    filename:join(Dir, Base ++ Suffix).

outfile(Base, Ext, Opts) when is_atom(Ext) ->
    outfile(Base, atom_to_list(Ext), Opts);
outfile(Base, Ext, Opts) ->
    Obase = case keyfind(outdir, 1, Opts) of
		{outdir, Odir} -> filename:join(Odir, Base);
		_Other -> Base			% Not found or bad format
	    end,
    Obase ++ "." ++ Ext.

objfile(Base, St) ->
    outfile(Base, "beam", St#compile.options).

tmpfile(Ofile) ->
    reverse([$#|tl(reverse(Ofile))]).

%% pre_defs(Options)
%% inc_paths(Options)
%%  Extract the predefined macros and include paths from the option list.

pre_defs([{d,M,V}|Opts]) ->
    [{M,V}|pre_defs(Opts)];
pre_defs([{d,M}|Opts]) ->
    [M|pre_defs(Opts)];
pre_defs([_|Opts]) ->
    pre_defs(Opts);
pre_defs([]) -> [].

inc_paths(Opts) ->
    [ P || {i,P} <- Opts, is_list(P) ].

src_listing(Ext, St) ->
    listing(fun (Lf, {_Mod,_Exp,Fs}) -> do_src_listing(Lf, Fs);
		(Lf, Fs) -> do_src_listing(Lf, Fs) end,
	    Ext, St).

do_src_listing(Lf, Fs) ->
    foreach(fun (F) -> io:put_chars(Lf, [erl_pp:form(F),"\n"]) end,
	    Fs).

listing(Ext, St) ->
    listing(fun(Lf, Fs) -> beam_listing:module(Lf, Fs) end, Ext, St).

listing(LFun, Ext, St) ->
    Lfile = outfile(St#compile.base, Ext, St#compile.options),
    case file:open(Lfile, [write,delayed_write]) of
	{ok,Lf} ->
            Code = restore_expanded_types(Ext, St#compile.code),
	    LFun(Lf, Code),
	    ok = file:close(Lf),
	    {ok,St};
	{error,_Error} ->
	    Es = [{Lfile,[{none,compile,write_error}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

restore_expanded_types("P", Fs) ->
    epp:restore_typed_record_fields(Fs);
restore_expanded_types("E", {M,I,Fs0}) ->
    Fs1 = restore_expand_module(Fs0),
    Fs = epp:restore_typed_record_fields(Fs1),
    {M,I,Fs};
restore_expanded_types(_Ext, Code) -> Code.

restore_expand_module([{attribute,Line,type,[Type]}|Fs]) ->
    [{attribute,Line,type,Type}|restore_expand_module(Fs)];
restore_expand_module([{attribute,Line,opaque,[Type]}|Fs]) ->
    [{attribute,Line,opaque,Type}|restore_expand_module(Fs)];
restore_expand_module([{attribute,Line,spec,[Arg]}|Fs]) ->
    [{attribute,Line,spec,Arg}|restore_expand_module(Fs)];
restore_expand_module([{attribute,Line,callback,[Arg]}|Fs]) ->
    [{attribute,Line,callback,Arg}|restore_expand_module(Fs)];
restore_expand_module([F|Fs]) ->
    [F|restore_expand_module(Fs)];
restore_expand_module([]) -> [].

shorten_filename(Name0) ->
    {ok,Cwd} = file:get_cwd(),
    case lists:prefix(Cwd, Name0) of
	false -> Name0;
	true ->
	    case lists:nthtail(length(Cwd), Name0) of
		"/"++N -> N;
		N -> N
	    end
    end.
