-module(bot_simulation).
-export([
    start/0, stop/0,
    create_tables/0, populate_grid/0, spawn_bots/0,
    request_path/2, move_bot/1, get_grid/0, get_bots/0
]).

-record(grid, {x, y, occupied_by}).
-record(bots, {id, position, path}).

start() ->
    mnesia:start(),
    create_tables(),
    populate_grid(),
    spawn_bots(),
    ok.

stop() -> mnesia:stop().

create_tables() ->
    mnesia:create_table(grid, [
        {attributes, record_info(fields, grid)},
        {type, set}
    ]),
    mnesia:create_table(bots, [
        {attributes, record_info(fields, bots)},
        {type, set}
    ]).

populate_grid() ->
    mnesia:transaction(fun() ->
        [mnesia:write(#grid{x = X, y = Y, occupied_by = none}) ||
         X <- lists:seq(1, 10), Y <- lists:seq(1, 10)]
    end).

spawn_bots() ->
    Positions = find_free_positions(5),
    mnesia:transaction(fun() ->
        lists:foreach(fun({BotID, {X, Y}}) ->
            mnesia:write(#bots{id = BotID, position = {X, Y}, path = []}),
            mnesia:write(#grid{x = X, y = Y, occupied_by = BotID})
        end, Positions)
    end).

find_free_positions(Count) ->
    Available = mnesia:dirty_match_object(#grid{x = '_', y = '_', occupied_by = none}),
    Selected = lists:sublist(shuffle(Available), Count),
    [{I, {X, Y}} || {X, Y, _} <- Selected, I <- lists:seq(1, Count)].

shuffle(List) -> lists:sort(fun(_, _) -> rand:uniform(2) =:= 1 end, List).

request_path(BotID, Destination) ->
    mnesia:transaction(fun() ->
        case mnesia:read(bots, BotID) of
            [#bots{id = BotID, position = {X, Y}, path = _}] ->
                Path = find_path({X, Y}, Destination),
                case reserve_path(Path) of
                    ok ->
                        mnesia:write(#bots{id = BotID, position = {X, Y}, path = Path}),
                        io:format("Bot ~p path reserved: ~p~n", [BotID, Path]),
                        ok;
                    collision -> {error, path_blocked}
                end;
            _ -> {error, bot_not_found}
        end
    end).

reserve_path(Path) ->
    case lists:any(fun({X, Y}) -> is_occupied(X, Y) end, Path) of
        true -> collision;
        false -> 
            lists:foreach(fun({X, Y}) ->
                mnesia:write(#grid{x = X, y = Y, occupied_by = reserved})
            end, Path),
            ok
    end.

is_occupied(X, Y) ->
    case mnesia:dirty_read(grid, {X, Y}) of
        [#grid{occupied_by = none}] -> false;
        _ -> true
    end.

move_bot(BotID) ->
    mnesia:transaction(fun() ->
        case mnesia:read(bots, BotID) of
            [#bots{id = BotID, position = _, path = [Next | Rest]}] ->
                update_bot_position(BotID, Next, Rest),
                io:format("Bot ~p moved to ~p~n", [BotID, Next]),
                ok;
            _ -> {error, no_path}
        end
    end).

update_bot_position(BotID, {X, Y}, Rest) ->
    case is_occupied(X, Y) of
        false ->
            mnesia:write(#bots{id = BotID, position = {X, Y}, path = Rest}),
            mnesia:write(#grid{x = X, y = Y, occupied_by = BotID}),
            ok;
        true -> {error, collision_detected}
    end.

find_path(Start, Goal) ->
    bfs([[Start]], Goal, []).

bfs([[Goal | Path] | _], Goal, _) -> lists:reverse([Goal | Path]);
bfs([Path | Rest], Goal, Visited) ->
    [Current | _] = Path,
    Neighbors = valid_moves(Current, Visited),
    bfs(Rest ++ [[N | Path] || N <- Neighbors], Goal, [Current | Visited]);
bfs([], _, _) -> [].

valid_moves({X, Y}, Visited) ->
    Moves = [{X+1, Y}, {X-1, Y}, {X, Y+1}, {X, Y-1}],
    lists:filter(fun({NX, NY}) ->
        NX >= 1, NX =< 10, NY >= 1, NY =< 10, not lists:member({NX, NY}, Visited)
    end, Moves).

get_grid() ->
    mnesia:transaction(fun() ->
        mnesia:match_object(#grid{x = '_', y = '_', occupied_by = '_'})
    end).

get_bots() ->
    mnesia:transaction(fun() ->
        mnesia:match_object(#bots{id = '_', position = '_', path = '_'})
    end).
