-module(phone_mnesia).
-export([setup/2]).
-include("phone_records.hrl").

setup(PhoneCallRecordFilename, CustomerRecordFilename) ->
    fill_table(phone_call, PhoneCallRecordFilename, fun add_phone_record/1, record_info(fields, phone_record), bag),
    fill_table(customer, CustomerRecordFilename, fun add_customer_record/1, record_info(fields, customer_record), set).

fill_table(TableName, Filename, AddDataFn, RecordInfo, TableType) ->
    mnesia:create_table(TableName, 
                        [{attributes, RecordInfo},
                         {type, TableType}]),
    {ok, IoDevice} = file:open(Filename, [read]),
    write_record(IoDevice, AddDataFn).

write_record(IoDevice, AddDataFn) ->
    case io:get_line(IoDevice, "") of
        eof ->
            ok;
        Line -> mnesia:transaction(fun () -> mnesia:write(AddDataFn(Line)) end),
                write_record(IoDevice, AddDataFn)
    end.

add_phone_record(Line) ->
    parse_phone_record(Line).

parse_phone_record(Line) ->
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

add_customer_record(Line) ->
    parse_customer_record(Line).

parse_customer_record(Line) ->
    StrippedLine = string:strip(Line, right, hd("\n")),
    [PhoneNumber, FirstName, LastName, MiddleName, RateRaw] = re:split(StrippedLine, ",", [{return, list}]),
    #customer_record{phone_number=PhoneNumber,
                     last_name=LastName,
                     first_name=FirstName,
                     middle_name=MiddleName,
                     rate=parse_raw_rate(RateRaw)}.

parse_raw_rate(RateRaw) ->
    {Rate, []} = string:to_float(RateRaw),
    Rate.
