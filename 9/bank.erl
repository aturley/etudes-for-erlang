-module(bank).

-export([account/1, get_command/0]).

account(Balance) ->
    Command = get_command(),
    account(Balance, Command).

get_command() ->
    Action = get_action(),
    case get_arg(Action) of
        invalid -> {invalid, none};
        Arg -> {Action, Arg}
    end.

get_action() ->
    case hd(io:get_line("D)eposit, W)ithdraw, B)alance, Q)uit: ")) of
        $D ->
            deposit;
        $W ->
            withdraw;
        $B ->
            balance;
        $Q ->
            quit;
        _ ->
            invalid
    end.

get_arg(deposit) ->
    read_number("Amount to deposit: ");
get_arg(withdraw) ->
    read_number("Amount to withdraw: ");
get_arg(balance) ->
    none;
get_arg(quit) ->
    none;
get_arg(invalid) ->
    none.

read_number(Prompt) ->
    {ok, String} = io:fread(Prompt, "~s"),
    string_to_number(hd(String)).

string_to_number(String) ->
    string_to_number([fun string:to_float/1, fun string:to_integer/1], String).
string_to_number([Fun | Funs], String) ->
    case Fun(String) of
        {error, _} ->
            string_to_number(Funs, String);
        {Result, _} ->
            Result * 1.0
    end;
string_to_number([], _) ->
    invalid.

account(_, {quit, _}) ->
    quit;
account(Balance, {Action, Arg}) ->
    case Action of
        deposit ->
            deposit(Arg, Balance);
        withdraw ->
            withdraw(Arg, Balance);
        balance ->
            balance(Arg, Balance);
        _ ->
            bad_input(Balance)
    end;
account(Balance, _) ->
    bad_input(Balance).

deposit(Arg, Balance) when Arg < 0 ->
    error_logger:error_msg("Negative deposit amount ~p~n", [Arg]),
    account(Balance);
deposit(Arg, Balance) when Arg >= 10000 ->
    error_logger:warning_msg("Excessive deposit ~p~n", [Arg]),
    account(Arg + Balance);
deposit(Arg, Balance) ->
    error_logger:info_msg("Successful deposit ~p~n", [Arg]),
    account(Arg + Balance).

withdraw(Arg, Balance) when Arg < 0 ->
    error_logger:error_msg("Negative withdrawl amount ~p~n", [Arg]),
    account(Balance);
withdraw(Arg, Balance) when Arg > Balance ->
    error_logger:error_msg("Overdraw ~p from balance ~p~n", [Arg, Balance]),
    account(Balance);
withdraw(Arg, Balance) ->
    error_logger:info_msg("Successful withdrawl ~p~n", [Arg]),
    account(Balance - Arg).

balance(_, Balance) ->
    error_logger:info_msg("Balance inquiry ~p~n", [Balance]),
    account(Balance).

bad_input(Balance) ->
    error_logger:error_msg("Bad input.", []),
    account(Balance).
