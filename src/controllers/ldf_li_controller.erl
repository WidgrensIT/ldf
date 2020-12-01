-module(ldf_li_controller).
-export([
         manage_li/1,
         delete_li/1
        ]).

manage_li(#{req := #{method := <<"POST">>},
            json := #{<<"value">> := Value,
                      <<"type">> := Type}}) ->
    Object = ldf_srv:add_li(Type, Value),
    {json, 201, #{}, Object};
manage_li(#{req := #{method := <<"GET">>}}) ->
    List = ldf_srv:get_all_li(),
    {json, 200, #{}, List}.

delete_li(#{req := #{method := <<"DELETE">>,
                     bindings := #{liid := Liid}}}) ->
    ldf_srv:remove_li(Liid),
    {status, 200}.