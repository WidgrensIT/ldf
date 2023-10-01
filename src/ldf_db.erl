-module(ldf_db).

-export([
    get_all_li/0,
    get_li_user_id/1,
    add_li/7,
    find_li/2,
    remove_li/1,
    add_message/2,
    get_messages/0,
    get_message/1
]).

get_all_li() ->
    SQL = <<"SELECT * FROM li">>,
    query(SQL, []).

get_li_user_id(UserId) ->
    SQL = <<"SELECT * FROM li WHERE user_id = $1">>,
    query1(SQL, [UserId]).

find_li(phone_number, PhoneNumber) ->
    SQL = <<"SELECT * FROM li WHERE phone_number = $1">>,
    query1(SQL, [PhoneNumber]);
find_li(email, Email) ->
    SQL = <<"SELECT * FROM li where email = $1">>,
    query1(SQL, [Email]).

add_li(Type, Value, CallbackId, UserId, Username, PhoneNumber, Email) ->
    PhoneLi =
        case find_li(phone_number, PhoneNumber) of
            {ok, Li2} -> Li2;
            undefined -> undefined
        end,
    EmailLi =
        case find_li(email, Email) of
            {ok, Li} -> Li;
            undefined -> undefined
        end,
    case {PhoneLi, EmailLi} of
        {undefined, undefined} ->
            SQL =
                <<"INSERT INTO li (type,\n"
                "                                       value,\n"
                "                                       callback_id,\n"
                "                                       user_id,\n"
                "                                       username,\n"
                "                                       phone_number,\n"
                "                                       email)\n"
                "                        VALUES ($1, $2, $3, $4, $5, $6, $7)">>,
            query1(SQL, [Type, Value, CallbackId, UserId, Username, PhoneNumber, Email]);
        _ ->
            ok
    end.

remove_li(Id) ->
    SQL = <<"DELETE FROM li WHERE callback_id = $1">>,
    query1(SQL, [Id]).

add_message(Payload, MessageId) ->
    SQL = <<"INSERT INTO ldf_message (payload, message_id) VALUES ($1, $2)">>,
    query1(SQL, [Payload, MessageId]).

get_messages() ->
    SQL = <<"SELECT payload FROM ldf_message">>,
    query(SQL, []).

get_message(MessageId) ->
    SQL = <<"SELECT payload FROM ldf_message WHERE message_id=$1">>,
    query(SQL, [MessageId]).

% Expect 1 result
query1(SQL, Values) ->
    case pgo:query(SQL, Values) of
        #{
            command := insert,
            num_rows := 1
        } ->
            ok;
        #{
            command := select,
            rows := []
        } ->
            undefined;
        #{
            command := select,
            rows := [Row]
        } ->
            {ok, Row};
        #{
            command := update,
            num_rows := Num
        } ->
            {ok, Num};
        #{
            command := delete,
            num_rows := 1
        } ->
            ok;
        #{command := delete} ->
            undefined;
        {error, Error} ->
            logger:error("Error: ~p on SQL ~p Values ~p", [Error, SQL, Values]),
            {error, Error}
    end.

query(SQL, Values) ->
    case pgo:query(SQL, Values) of
        #{
            command := insert,
            num_rows := Num
        } ->
            {ok, Num};
        #{
            command := select,
            rows := Rows
        } ->
            {ok, Rows};
        #{
            command := update,
            num_rows := Num
        } ->
            {ok, Num};
        #{
            command := delete,
            num_rows := Num
        } ->
            {ok, Num};
        {error, Error} ->
            logger:error("Error: ~p on SQL ~p Values ~p", [Error, SQL, Values]),
            {error, Error}
    end.
