-module(v).
-compile([export_all]).

%% ========================================================================
%% Example by core
%% ========================================================================
printer(X) -> io:format("~p~n",[X]), core:become(fun printer/1).

fact({N, C}) ->
    if N == 0 -> C ! 1;
       true   -> core:self() ! {N-1, core:new(fun(V) -> C ! N * V end)}
    end,
    core:become(fun fact/1).

%% (without module function)
%% > FactPrim = fun(F) -> fun(Arg) -> case Arg of {N, C} -> if N == 0 -> C ! 1; true -> self() ! {N-1, core:new(fun(V) -> C ! N * V end)} end, core:become(F) end end end.
%% > Fact = utils:y(FactPrim).
%% > Printer = core:new(fun v:printer/1).
%% > Actor = core:new(Fact).
%% > Actor ! {10, Printer}.
%% {10,<0.59.0>}


%% ========================================================================
%% Example by runtime
%% ========================================================================
printerM(X, Self) -> io:format("~p~n",[X]).

factM({N, C}, Self) ->
    if N == 0 -> runtime:send(C, 1);
       true   -> runtime:send(Self, {N-1, runtime:new(fun(V, Self) -> runtime:send(C, N * V) end)})
    end.

%% > runtime:start().
%% > Printer = runtime:new(fun v:printerM/2).
%% > Fact = runtime:new(fun v:factM/2).
%% > Fact ! {mesg, {10, Printer}}.
%% {mesg,{10,<0.42.0>}}
%% 3628800

%% - 引数に入れる
%% - 動的スコープみたいな機構を用意する
%% - システムに問い合わせる（システム側で今実行中のアクターはどれか把握して、返せるようにしておく）
%% - プロセス辞書のようなものを用意する

%% - こういうメタ機構を容易する場合、ユーザー定義のノーテーションだが組み込み構文っぽく見せられる機構（あるいは、ある種の演算子の意味を再定義する機能）が必須（e.g. runtime:send/2）

%% - エラー時に正しく状態を元に戻す処理
