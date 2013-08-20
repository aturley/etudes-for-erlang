-module(phone_ets).
-export([setup/1, summary/1, summary/0]).
-include("phone_records.hrl").

setup(FileName) ->
    PhoneRecordTable = ets:new(phone_records, [named_table, 
                                               duplicate_bag,
                                               {keypos, #phone_record.phone_number}]),
    {ok, IoDevice} = file:open(FileName, [read]),
    read_file(IoDevice, PhoneRecordTable).

read_file(IoDevice, PhoneRecordTable) ->
    case io:get_line(IoDevice, "") of
        eof  -> PhoneRecordTable;
        Line -> ets:insert(PhoneRecordTable, parse_line(Line)),
                read_file(IoDevice, PhoneRecordTable)
    end.

parse_line(Line) ->
    StrippedLine = string:strip(Line, right, hd("\n")),
    [PhoneNumber, StartingDateRaw, StartingTimeRaw, EndingDateRaw, EndingTimeRaw] = re:split(StrippedLine, ",", [{return, list}]),
    #phone_record{phone_number=PhoneNumber,
                  starting_date=parse_raw_date(StartingDateRaw),
                  starting_time=parse_raw_time(StartingTimeRaw),
                  ending_date=parse_raw_date(EndingDateRaw),
                  ending_time=parse_raw_time(EndingTimeRaw)}.

parse_raw_date(RawDate) ->
    [{Y, []}, {M, []}, {D, []}] = [string:to_integer(X) || X <- re:split(RawDate, "-", [{return, list}])],
    {Y, M, D}.

parse_raw_time(RawTime) ->
    [{H, []}, {M, []}, {S, []}] = [string:to_integer(X) || X <- re:split(RawTime, ":", [{return, list}])],
    {H, M, S}.

summary(PhoneNumber) ->
    {PhoneNumber, phone_records_to_total_minutes(ets:lookup(phone_records, PhoneNumber))}.

summary() ->
    First = ets:first(phone_records),
    summary(First, []).

summary('$end_of_table', Accumulator) ->
    Accumulator;
summary(Key, Accumulator) ->
    summary(ets:next(phone_records, Key), [summary(Key) | Accumulator]).

phone_records_to_total_minutes(PhoneRecords) ->
    lists:foldl(fun (X, Y) -> phone_record_to_total_minutes(X) + Y end, 0, PhoneRecords).

phone_record_to_total_minutes(PhoneRecord) ->
    StartSeconds = calendar:datetime_to_gregorian_seconds({PhoneRecord#phone_record.starting_date,
                                                           PhoneRecord#phone_record.starting_time}),
    EndSeconds = calendar:datetime_to_gregorian_seconds({PhoneRecord#phone_record.ending_date,
                                                         PhoneRecord#phone_record.ending_time}),
    ((EndSeconds - StartSeconds) + 59) div 60.
