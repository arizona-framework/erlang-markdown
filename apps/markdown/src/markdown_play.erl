-module(markdown_play).

-export([
    camel_to_snake/1
]).

camel_to_snake(Bin) when is_binary(Bin) ->
    % Convert binary to list of characters for easier processing
    Chars = binary_to_list(Bin),
    
    % Process the characters with additional context tracking
    SnakeCase = camel_to_snake_impl(Chars, [], undefined, undefined),
    
    % Convert back to binary
    list_to_binary(SnakeCase).

% Add parameters to track the current pattern
% PrevChar is used to detect the end of an acronym
camel_to_snake_impl([], Acc, _PrevCase, _PrevChar) ->
    % Reverse the accumulated result (since we've been prepending)
    lists:reverse(Acc);

% First character of the string
camel_to_snake_impl([H|T], [], _PrevCase, _PrevChar) when H >= $A, H =< $Z ->
    % First character is uppercase, convert to lowercase
    camel_to_snake_impl(T, [H + 32], upper, H);

% Handle transition from acronym to lowercase (like "APIc")
camel_to_snake_impl([H|T], Acc, upper, _PrevChar) when H >= $a, H =< $z ->
    % If previous was uppercase and current is lowercase, we're at the end of an acronym
    % or a camel case word - add underscore unless this is the first lowercase after
    % a single uppercase (like "Abc")
    case Acc of
        [_, $_ | _] ->
            % If we've already added an underscore, don't add another
            camel_to_snake_impl(T, [H | Acc], lower, H);
        _ ->
            % Check if we're at the boundary of an acronym (multiple uppercase followed by lowercase)
            Peek = if
                length(Acc) >= 2 ->
                    % Check if the last two characters were both uppercase (part of an acronym)
                    % We can check this by seeing if both are lowercase in our accumulator
                    % (since we've already converted them)
                    [LastChar, SecondLastChar | _] = Acc,
                    LastChar >= $a andalso LastChar =< $z andalso 
                    SecondLastChar >= $a andalso SecondLastChar =< $z;
                true ->
                    false
            end,
            
            % Add underscore if we're at the acronym boundary
            if
                Peek ->
                    camel_to_snake_impl(T, [H, $_ | Acc], lower, H);
                true ->
                    camel_to_snake_impl(T, [H | Acc], lower, H)
            end
    end;

% Handle uppercase letter after lowercase (like "aB")
camel_to_snake_impl([H|T], Acc, lower, _PrevChar) when H >= $A, H =< $Z ->
    % Add underscore before converting uppercase to lowercase
    camel_to_snake_impl(T, [H + 32, $_ | Acc], upper, H);

% Handle uppercase letter after uppercase (like "AB")
camel_to_snake_impl([H|T], Acc, upper, _PrevChar) when H >= $A, H =< $Z ->
    % For consecutive uppercase, don't add underscore yet
    camel_to_snake_impl(T, [H + 32 | Acc], upper, H);

% Handle any other character
camel_to_snake_impl([H|T], Acc, _PrevCase, _PrevChar) when H >= $a, H =< $z ->
    camel_to_snake_impl(T, [H | Acc], lower, H);
camel_to_snake_impl([H|T], Acc, PrevCase, _PrevChar) ->
    camel_to_snake_impl(T, [H | Acc], PrevCase, H).
