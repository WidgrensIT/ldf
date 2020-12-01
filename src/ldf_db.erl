-module(ldf_db).

-export([get_all_li/0,
         add_li/3,
         remove_li/1,
         add_message/1,
         get_messages/0]).


get_all_li() ->
    SQL = <<"SELECT * FROM li">>,
    query(SQL, []).

add_li(Type, Value, CallbackId) ->
    SQL = <<"INSERT INTO li (type, value, callback_id) VALUES ($1, $2, $3)">>,
    query1(SQL, [Type, Value, CallbackId]).

remove_li(Id) ->
    SQL = <<"DELETE FROM li WHERE callback_id = $1">>,
    query1(SQL, [Id]).

add_message(Payload) ->
    SQL = <<"INSERT INTO ldf_message (payload) VALUES ($1)">>,
    query1(SQL, [Payload]).

get_messages() ->
    SQL = <<"SELECT payload FROM ldf_message">>,
    query(SQL, []).

% Expect 1 result
query1(SQL, Values) ->
    case pgo:query(SQL, Values) of
        #{command := insert,
          num_rows := 1} -> ok;
        #{command := select,
          rows := []} -> undefined;
        #{command := select,
          rows := [Row]} -> {ok, Row};
        #{command := update,
          num_rows := Num} -> {ok, Num};
        #{command := delete,
          num_rows := 1} -> ok;
        #{command := delete} -> undefined;
        {error, Error} ->
            logger:error("Error: ~p on SQL ~p Values ~p", [Error, SQL, Values]),
            {error, Error}
    end.

query(SQL, Values) ->
    case pgo:query(SQL, Values) of
        #{command := insert,
          num_rows := Num} -> {ok, Num};
        #{command := select,
          rows := Rows} -> {ok, Rows};
        #{command := update,
          num_rows := Num} -> {ok, Num};
        #{command := delete,
          num_rows := Num} -> {ok, Num};
        {error, Error} ->
            logger:error("Error: ~p on SQL ~p Values ~p", [Error, SQL, Values]),
            {error, Error}
    end.