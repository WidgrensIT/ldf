-module(ldf_li_controller).
-export([
         create_li/1,
         manage_li/1,
         delete_li/1,
         manage_history/1
        ]).

create_li(#{json := #{<<"value">> := Value,
                      <<"type">> := Type}}) ->
    Object = ldf_srv:add_li(Type, Value),
    {json, 201, #{}, Object}.

manage_li(_) ->
    List = ldf_srv:get_all_li(),
    {json, 200, #{}, List}.

delete_li(#{bindings := #{<<"liid">> := Liid}}) ->
    ldf_srv:remove_li(Liid),
    {status, 200}.

manage_history(#{json := Json}) ->
    ldf_srv:get_history(json:encode(Json, [maps, binary])),
    {status, 200}.