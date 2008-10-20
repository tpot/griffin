-module(xml).

-export([get_attr/2, get_attr/3]).

-include_lib("xmerl/include/xmerl.hrl").

%% Fetch optional attribute
%%
%% @spec get_attr(atom(), xmlAttribute(), string()) -> string()

get_attr(Name, Elt, DefaultValue) ->
    PropList = [proplists:property(Attr#xmlAttribute.name, Attr) ||
                   Attr <- Elt#xmlElement.attributes],
    proplists:get_value(Name, PropList, #xmlAttribute{value = DefaultValue}).

%% Fetch required attribute
%%
%% @spec get_attr(atom(), xmlAttribute()) ->
%%           string() | throw({error, {missing_attribute_error, string()}})

get_attr(Name, Elt) ->
    case get_attr(Name, Elt, undefined) of
        #xmlAttribute{value = undefined} -> 
            throw({error, {missing_attribute_error, 
                           io_lib:format("Missing ~p attribute", [Name])}});
        Any -> 
            Any
    end.
