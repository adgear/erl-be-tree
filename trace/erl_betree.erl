-module(erl_betree).

%%
%% Module to facilitate FAQ - How to trace functions in remote Erlang node.
%% Mocks 'erl_betree:betree_make_event/3'.
%%

%% API
-export([
  betree_make_event/3
]).

betree_make_event(P1, P2, P3) ->
  P1T = P1, P2T = P2, P3T = P3,
  timer:sleep(100),
  {P1T, P2T, P3T}.