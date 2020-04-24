unit binU;

{$MODE Delphi}

interface

function setbit(bitword: longword; bitPlace : longword): longword;

function isbitset(bitword: longword; bitPlace: longword): boolean;

//function fromPostfixToInfix(Postfix: array of array of string): string;

implementation

function setbit(bitword: longword; bitPlace : longword): longword;
var
  i: integer;
begin
  result := 1;
  if bitPlace = 0 then exit;
  for i := 0 to bitPlace - 1 do result := result * 2;
  result := bitword or result;
end;

function isbitset(bitword: longword; bitPlace: longword): boolean;
begin
  result := (bitWord and setbit(0,bitPlace)) > 0;
end;

{
function fromPostfixToInfix(Postfix: array of array of string): string;
var
  i, j: Integer;
  st: string;
  stack: array of array of string = nil;
  stk: array [0..1] of string;
begin
  for i := 0 to length(Postfix) do
    for j := low(Postfix[i]) to high(Postfix[i]) do
      begin
        st := Postfix[i,j];
        if (st = 'ADD') or (st = 'SUB') or (st = 'MUL') or (st = 'DIV') then
          begin
            if length(stack) < 2 then //error
             else
              begin
                setlength(stack,length(stack)+1);

                stack[high(stack)] :=
              end;

          end;


      end;
end;

  while there are input symbol left
  2.	Read the next symbol from input.
  3.	If the symbol is an operand
  		Push it onto the stack.
  4.	Otherwise,
  		the symbol is an operator.
  5.	If there are fewer than 2 values on the stack
  		Show Error /* input not sufficient values in the expression */
  6.	Else
  		Pop the top 2 values from the stack.
  		Put the operator, with the values as arguments and form a string.
  		Encapsulate the resulted string with parenthesis.
  		Push the resulted string back to stack.
  7.	If there is only one value in the stack
  		That value in the stack is the desired infix string.
  8.	If there are more values in the stack
  		Show Error /* The user input has too many values */

}
end.
