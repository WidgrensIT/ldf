-module(ldf_receiver_controller).
-export([
         incoming_message/1
        ]).

incoming_message(#{req := #{method := <<"POST">>},
                   json := Json}) ->
    logger:debug("incoming message: ~p", [Json]),
    Result = etsi103707:json_to_xml(Json),
    logger:debug(Result),
    {status, 200}.