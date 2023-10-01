-module(ldf_router).

-export([routes/1]).

routes(_Env) ->
    [
        #{
            prefix => "",
            security => false,
            routes => [
                {"/receiver", {ldf_receiver_controller, create_message}, #{methods => [post]}},
                {"/receiver", {ldf_receiver_controller, get_message}, #{methods => [get]}},
                {"/message/:messageid", {ldf_message_controller, message}, #{methods => [get]}},
                {"/li", {ldf_li_controller, create_li}, #{methods => [post]}},
                {"/li", {ldf_li_controller, manage_li}, #{methods => [get]}},
                {"/history", {ldf_li_controller, manage_history}, #{methods => [post]}},
                {"/li/:liid", {ldf_li_controller, delete_li}, #{methods => [delete]}},
                {"/www/admin", "assets/admin.html"},
                {"/www/receiver", "assets/receiver.html"},
                {"/assets/[...]", "assets"}
            ]
        }
    ].
