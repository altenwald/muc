-module(muc_presence).
-compile([warnings_as_errors]).

-export([
    create_room/2
]).

-include_lib("exmpp/include/exmpp.hrl").

create_room(From, To) ->
    exmpp_xml:element('jabber:client', 'presence', [
        exmpp_xml:attribute(<<"from">>, From),
        exmpp_xml:attribute(<<"to">>, To)
    ],[
        exmpp_xml:element(?NS_MUC_USER, 'x', [], [
            exmpp_xml:element(undefined, 'item', [
                exmpp_xml:attribute(<<"jid">>, To),
                exmpp_xml:attribute(<<"affiliation">>, <<"owner">>),
                exmpp_xml:attribute(<<"role">>, <<"moderator">>)
            ]),
            exmpp_xml:element(undefined, 'status', [
                exmpp_xml:attribute(<<"code">>, <<"110">>)
            ]),
            exmpp_xml:element(undefined, 'status', [
                exmpp_xml:attribute(<<"code">>, <<"201">>)
            ]) 
        ])
    ]).
