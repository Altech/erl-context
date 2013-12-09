-module(v).
-compile([export_all]).

%%% ========================================================================
%%% Example by core
%%% ========================================================================
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


%%% ========================================================================
%%% Example by runtime
%%% ========================================================================
printerM(X, _) -> io:format("~p~n",[X]).

factM({N, C}, Self) ->
    if N == 0 -> runtime:send(C, 1);
       true   -> runtime:send(Self, {N-1, runtime:new(fun(V, _) -> runtime:send(C, N * V) end)})
    end.

%% Per-Actor
%% > runtime:start().
%% > Printer = runtime:new(fun v:printerM/2).
%% > Fact = runtime:new(fun v:factM/2).
%% > runtime:send(Fact, {10, Printer}).
%% {mesg,{10,<0.42.0>}}
%% 3628800

%% A Composed Group-Wide Metalevel
%% > runtime:start().
%% > PrinterG = runtime:newG([fun v:printerM/2]).
%% > FactG = runtime:newG([fun v:factM/2]).
%% > Printer = {1, PrinterG}.
%% > Fact = {1, FactG}.
%% > runtime:send(Fact, {10, Printer}).

%% Q. 自分自身のメタアクターをどう認識させるか？（メタアクターとどう通信するか？）
%% - 引数に入れる
%% - 動的スコープみたいな機構を用意する
%% - システムに問い合わせる（システム側で今実行中のアクターはどれか把握して、返せるようにしておく。予めシステムの名前を決め打ちする必要。）
%%   - 欠点：問い合わせサーバーにリクエストが集中する
%% - プロセス辞書のようなものを用意する
%%   - 利点：システム=実行を担当するプロセス（アクター）なので、実行を担当するアクターを増やすのも自由

%% Q. アクターを終了させたい場合はどうするか？
%% - メタアクターに対して、そういうメッセージを送る
%% - self-become制限のないものと同じ表現力があるようにできると良い（behaviorの更新など）

%% Q. execアクターは一つで構わないのか？
%% - 各アクターごとに作っても特に問題なさそう

%% - こういうメタ機構を実際に使う場合、ユーザー定義のノーテーションだが組み込み構文っぽく見せられる機構（あるいは、ある種の演算子の意味を再定義する機能）が必須（e.g. runtime:send/2）

%% - エラー時に正しく状態を元に戻す処理
