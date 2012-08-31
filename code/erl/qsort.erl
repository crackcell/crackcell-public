-module(qsort).
-export([qsort/1]).

qsort([]) ->
	[];
qsort([Pivot|Rest]) ->
	qsort([Front || Front <- Rest, Front < Pivot])
		++ [Pivot] ++
		qsort([Back || Back <- Rest, Back >= Pivot]).


