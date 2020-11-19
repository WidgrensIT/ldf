-module(etsi103707).

-export([json_to_xml/1]).


json_to_xml(Object) ->
    logger:debug("converting to 103 707"),
    Object.