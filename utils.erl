-module(utils).
-compile(export_all).

y(F) -> F (fun(X) -> (y (F)) (X) end).
