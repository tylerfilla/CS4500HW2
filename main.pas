{*
 * Tyler Filla
 * February 4, 2019
 * CS 4500-001 :: Intro to Software Profession
 *
 * This program intends to implement the specified game and program behavior: A
 * strongly-connected digraph is drawn on an imaginary game board, and a marker
 * is randomly moved along the edges until all nodes have been visited. Each
 * node counts the number of times it has been visited, and some statistics are
 * produced after the game completes.
 *
 * The words "node" and "circle" are used interchangeably. Also, the words
 * "arrow" and "edge" are used interchangeably.
 *
 * FILES: An input file containing a description of a strongly-connected digraph
 * must be supplied and named 'HW1infile.txt' in the current working directory.
 * A transcript will be produced in the file 'HW1fillaOutfile.txt'. These files
 * are considered mission critical, so I/O errors may cause early termination.
 *
 * INPUT FILE FORMAT: The input file must conform to a strict textual format:
 *   - One line containing the integer number N, the number of circles (nodes)
 *   - One line containing the integer number K, the number of arrows (edges)
 *   - K lines, one edge (arrow) per line, containing two integer numbers each
 *       - The first integer number, per line, is the source circle number
 *       - The second integer number, per line, is the destination circle number
 *
 * The program will make a good faith effort to identify issues in formatting;
 * however, not every pathological input file has been tested.
 *
 * GOOD INPUT FILE EXAMPLE:
 *  4
 *  5
 *  1 2
 *  2 1
 *  2 3
 *  3 4
 *  4 1
 *
 * In this example, four circles are described (N=4 on the first line). Five
 * arrows (K=5 on the second line) are to be provided by the five following
 * lines. The third line prescribes an arrow from circle 1 to circle 2, the
 * fourth line prescribes an arrow from circle 2 to circle 1, and so on.
 * Gameplay can proceed without error.
 *
 * BAD INPUT FILE EXAMPLE:
 *  4
 *  5
 *  1 2
 *  2 1
 *  2 3
 *  3 4
 *
 * This is an example of a bad input file whose formatting issues will be
 * identified by the program. As you can see, five arrows are prescribed (K=5 on
 * the second line), but only four are defined in the lines that follow. The
 * program will abort on account of premature EOF.
 *
 * INTERNALS: The program represents each circle (node) as a record with three
 * members: Number, Marks, and Arrows. The Number integer associates each circle
 * with its numeric index (starting with 1, not 0). The Marks integer counts the
 * total number of checkmarks placed on the circle. The Arrows member is a
 * dynamic array of pointers to other circles. Each arrow is represented in
 * memory as a pointer to the destination node stored inside the source node.
 *
 * Basic statistics are kept in global variables as the game progresses. These
 * include tallies on total- and uniquely-marked circles, as well as a counter
 * for the total number of checkmarks drawn (visits made) overall.
 *
 * NOTE: All program output, less unhandled exceptions, is mirrored to the
 * output file via a custom "MyWriteLn" function that composes two "WriteLn"
 * calls. I wished to find a redirection system in Pascal similar to C++'s
 * std::ios::rdbuf(...), but I was unable to. My final solution is less elegant,
 * but it works.
 *}

{ Enable exceptions if not already. }
{$I+}
{$mode objfpc}

program main;

uses sysutils, regexpr;

const
  { The name of the input file. }
  C_FILENAME_IN = 'HW1infile.txt';

  { The name of the output file. }
  C_FILENAME_OUT = 'HW1fillaOutfile.txt';

type
  {*
   * An exception for input file errors.
   *
   * This is a catch-all exception thrown by the input file parser procedure
   * when something doesn't go its way. Its message contains more details.
   *}
  EInputFileException = class(Exception);

  { Pointer to TCircle. See below. }
  TCirclePtr = ^TCircle;

  {*
   * A circle on the whiteboard.
   *
   * One of these records tracks the relevant state of exactly one circle drawn
   * on the imaginary whiteboard. Each circle knows its number (thus obviating
   * the need to iterate the global circle array), and each circle counts its
   * virtual checkmarks.
   *
   * The in-memory graph construction is based on simple pointers.
   *}
  TCircle = record
    { The number assigned to this circle. }
    Number: integer;

    { The number of checkmarks placed on the circle. }
    Marks: integer;

    { Connections to other circles (the arrows originating here). }
    Arrows: array of TCirclePtr;
  end;

var
  { The output text file. }
  OutputFile: TextFile;

  { The number N. The number of circles in play. }
  N: integer;

  { The number K. The number of arrows in play. }
  K: integer;

  { Tracker for all allocated circles. }
  AllCircles: array of TCircle;

  { A pointer to the current circle. }
  CurrentCircle: TCirclePtr;

  { The unique number of circles marked. }
  UniqueCirclesMarked: integer;

  { The total number of marks distributed. }
  TotalCircleMarks: integer;

  { The maximum number of marks in any one circle. }
  MaxSingleCircleMarks: integer;

{*
 * Write output to stdout and the output file.
 *
 * @param Text The output text
 *}
procedure MyWriteLn(const Text: string);
begin
  { To screen. Output is standard output. }
  WriteLn(Output, Text);

  { To file. OutputFile is program-global. }
  WriteLn(OutputFile, Text);
end;

{*
 * Set up the circles from the given input file.
 *
 * @param Name The input file name
 *}
procedure InitCirclesFromFile(const Name: string);
var
  { The opened input file. }
  InputFile: TextFile;

  { An iterator index over 1..K. }
  i: integer;

  { A temporary arrow string. }
  ArrowStr: string;

  { A regular expression to arrow details. }
  ArrowRegExpr: TRegExpr;

  { A temporary number of an arrow. }
  ArrowNum: integer;

  { A temporary index of an arrow's source circle. }
  ArrowSrc: integer;

  { A temporary index of an arrow's destination circle. }
  ArrowDest: integer;
begin
  MyWriteLn(Format('Initializing from input file %s', [Name]));

  { The input file. }
  AssignFile(InputFile, Name);

  { Create the regular expression for pulling out arrow details. }
  ArrowRegExpr := TRegExpr.Create;
  ArrowRegExpr.Expression := '\d+';

  try
    try
      { Open the input file for read. }
      Reset(InputFile);

      { Try to read variable N (number of circles). }
      if Eof(InputFile) then
        raise EInputFileException.create('Failed to read N: Premature EOF');
      try
        ReadLn(InputFile, N);
      except
        on E: Exception do
          raise EInputFileException.create(Format('Failed to read N: %s: %s', [E.ClassName, E.Message]));
      end;

      { Check range of N (from 2 to 10). }
      if (N < 2) or (N > 10) then
        raise EInputFileException.create(Format('N is out of range: %d', [N]));

      { Allocate N circles. }
      SetLength(AllCircles, N);

      { Initialize allocated circles. }
      for i := 1 to N do
        begin
          AllCircles[i - 1].Number := i;
          AllCircles[i - 1].Marks := 0;
          AllCircles[i - 1].Arrows := nil;
        end;

      { Try to read variable K (number of arrows). }
      if Eof(InputFile) then
        raise EInputFileException.create('Failed to read K: Premature EOF');
      try
        ReadLn(InputFile, K);
      except
        on E: Exception do
          raise EInputFileException.create(Format('Failed to read K: %s: %s', [E.ClassName, E.Message]));
      end;

      { Check range of K (from 2 to 100). }
      if (K < 2) or (K > 100) then
        raise EInputFileException.create(Format('K is out of range: %d', [K]));

      { Read in K arrow definitions. }
      for ArrowNum := 1 to K do
        begin
          { Try to read arrow string. }
          if Eof(InputFile) then
            raise EInputFileException.create(Format('Failed to read arrow %d: Premature EOF', [ArrowNum]));
          try
            ReadLn(InputFile, ArrowStr);
          except
            on E: Exception do
              raise EInputFileException.create(Format('Failed to read arrow %d: %s: %s', [ArrowNum, E.ClassName, E.Message]));
          end;

          { Pull source and destination indices from line. }
          if ArrowRegExpr.Exec(ArrowStr) then
            begin
              { Pull source. }
              try
                ArrowSrc := ArrowRegExpr.Match[0].ToInteger;
              except
                on E: Exception do
                  raise EInputFileException.create(Format('Failed to read arrow %d source: %s: %s', [ArrowNum, E.ClassName, E.Message]));
              end;

              { Pull destination. }
              ArrowRegExpr.ExecNext;
              try
                ArrowDest := ArrowRegExpr.Match[0].ToInteger;
              except
                on E: Exception do
                  raise EInputFileException.create(Format('Failed to read arrow %d destination: %s: %s', [ArrowNum, E.ClassName, E.Message]));
              end;
            end;

          { Check range of arrow source index (from 1 to N). }
          if (ArrowSrc < 1) or (ArrowSrc > N) then
            raise EInputFileException.create(Format('Source index for arrow %d is out of range: %d', [ArrowNum, ArrowSrc]));

          { Check range of arrow destination index (from 1 to N). }
          if (ArrowDest < 1) or (ArrowDest > N) then
            raise EInputFileException.create(Format('Destination index for arrow %d is out of range: %d', [ArrowNum, ArrowDest]));

          { Establish the arrow connection in memory. This increments the length
            of the source circle's arrow array and appends to it a pointer to
            the destination circle. }
          SetLength(AllCircles[ArrowSrc - 1].Arrows, Length(AllCircles[ArrowSrc - 1].Arrows) + 1);
          AllCircles[ArrowSrc - 1].Arrows[High(AllCircles[ArrowSrc - 1].Arrows)] := @AllCircles[ArrowDest - 1];
        end;

      { Set the first circle as current. }
      CurrentCircle := @AllCircles[0];
    except
      { Rethrow miscellaneous I/O errors under catch-all exception. }
      on E: EInOutError do
        raise EInputFileException.create(Format('Failed to read input file: %s: %s ', [E.ClassName, E.Message]));
    end;
  finally
    { Close the input file. }
    CloseFile(InputFile);

    { Free arrow regular expression. }
    ArrowRegExpr.Free;
  end;
end;

{*
 * Mark the current circle.
 *}
procedure MarkCurrentCircle;
begin
  { Increment global mark statistic. }
  Inc(TotalCircleMarks);

  { Increment the mark count on the current circle. }
  Inc(CurrentCircle^.Marks);

  { Update maximum mark count if needed. }
  if CurrentCircle^.Marks > MaxSingleCircleMarks then
    MaxSingleCircleMarks := CurrentCircle^.Marks;

  MyWriteLn(Format('Marked circle %d (up to %d mark(s))', [CurrentCircle^.Number, CurrentCircle^.Marks]));
end;

{*
 * Carry out the game.
 *}
procedure PlayGame;
var
  { The number of arrows from the current circle. This varies over gameplay. }
  NumArrows: integer;

  { The last circle we were in. }
  LastCircle: TCirclePtr;

  { The last randomly-chosen arrow index. }
  ChosenArrow: integer;
begin
  { Core gameplay loop. Stops after all circles have been marked. }
  while (UniqueCirclesMarked < N) do
    begin
      MyWriteLn('');
      MyWriteLn(Format('Currently in circle %d', [CurrentCircle^.Number]));

      { Count arrows leaving the current circle. }
      NumArrows := Length(CurrentCircle^.Arrows);

      MyWriteLn(Format('-> %d arrow(s) point away from circle %d', [NumArrows, CurrentCircle^.Number]));

      { If no arrows are available, exit with error. This should not happen if
        the input describes a strongly-connected digraph, but we handle the
        error anyway. The user might have made a typo in the input file. }
      if NumArrows = 0 then
        begin
          MyWriteLn(Format('FAIL: Stuck on circle %d: No arrows to follow', [CurrentCircle^.Number]));
          MyWriteLn('The configured graph is not strongly-connected! Bailing out...');
          Exit;
        end;

      { Remember the current circle. }
      LastCircle := CurrentCircle;

      { Randomly choose the next circle from the available arrows. }
      ChosenArrow := Random(NumArrows);
      CurrentCircle := CurrentCircle^.Arrows[ChosenArrow];

      MyWriteLn(Format('Moved marker from circle %d to circle %d via arrow %d', [LastCircle^.Number, CurrentCircle^.Number, ChosenArrow]));

      { Mark the new current circle. }
      MarkCurrentCircle();

      { If there was no mark on the circle to begin with... }
      if CurrentCircle^.Marks = 1 then
        begin
          { ... then we just hit it for the first time. Increment such count. }
          Inc(UniqueCirclesMarked);

          MyWriteLn(Format('-> Hit circle %d for the first time', [CurrentCircle^.Number]));
          MyWriteLn(Format('-> Visited %d unique circles out of %d so far', [UniqueCirclesMarked, N]));
        end;
    end;
end;

{ Program entry point. }
begin
  UniqueCirclesMarked := 0;
  TotalCircleMarks := 0;
  MaxSingleCircleMarks := 0;

  { The output file. This will be used to produce a transcript of the game. }
  AssignFile(OutputFile, C_FILENAME_OUT);

  { Try to open the output file for write (creating it if needed). }
  try
    Rewrite(OutputFile);
  except
    on E: EInOutError do
      begin
        WriteLn(Format('Failed to open output file for write: %s', [E.Message]));
        Exit;
      end;
  end;

  { Try to initialize from the input file. }
  try
    InitCirclesFromFile(C_FILENAME_IN);
  except
    on E: EInputFileException do
      begin
        MyWriteLn(Format('Input file error: %s', [E.Message]));
        CloseFile(OutputFile);
        Exit;
      end;
  end;

  { Get a new random sequence. }
  Randomize();

  MyWriteLn('Gameplay is about to begin.');
  MyWriteLn('');

  { Mark the first circle as current. }
  UniqueCirclesMarked := 1;
  MarkCurrentCircle();

  { Play the game. }
  PlayGame();

  MyWriteLn('');
  MyWriteLn('Gameplay is complete!');

  MyWriteLn('');
  MyWriteLn('~~~ LAST RUN STATISTICS ~~~');

  MyWriteLn(Format('I. There were N=%d circles in play. This was prescribed by the input file.', [N]));
  MyWriteLn(Format('II. There were K=%d arrow(s) in play. This was prescribed by the input file.', [K]));
  MyWriteLn(Format('III. In total, %d marks were distributed across all circles. Some circles may have received many marks depending on the graph construction.', [TotalCircleMarks]));
  MyWriteLn(Format('IV. On average, each circle received %f marks.', [TotalCircleMarks / N]));
  MyWriteLn(Format('V. In any one circle, the maxmimum number of marks was %d. All circles received at most this many marks during gameplay.', [MaxSingleCircleMarks]));

  { Close the output file. }
  CloseFile(OutputFile);
end.
