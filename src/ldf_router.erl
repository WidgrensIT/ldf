-module(ldf_router).

-export([routes/1]).

routes(_Env) ->
    [
        #{
            prefix => "",
            security => false,
            routes => [
                {"/receiver", fun ldf_receiver_controller:create_message/1, #{methods => [post]}},
                {"/receiver", fun ldf_receiver_controller:get_message/1, #{methods => [get]}},
                {"/message/:messageid", fun ldf_message_controller:message/1, #{methods => [get]}},
                {"/li", fun ldf_li_controller:create_li/1, #{methods => [post]}},
                {"/li", fun ldf_li_controller:manage_li/1, #{methods => [get]}},
                {"/history", fun ldf_li_controller:manage_history/1, #{methods => [post]}},
                {"/li/:liid", fun ldf_li_controller:delete_li/1, #{methods => [delete]}},
                {"/www/admin", "assets/admin.html"},
                {"/www/receiver", "assets/receiver.html"},
                {"/assets/[...]", "assets"}
            ]
        }
    ].
