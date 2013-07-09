string_to_number(String) ->
    Number = string:to_float(String),
    case Number of
        {error,_} ->
            {N, _} = string:to_integer(String), 
            N;
        {N, _} -> N
    end.


char_to_shape(Char)  ->
    
  case [Char] of
      "T" ->
          triangle;
      "t" -> triangle;
      "R" -> rectangle;
      "r" -> rectangle;
      "E" -> ellipse;
      "e" -> ellipse;
      _ -> unknown
  end.
