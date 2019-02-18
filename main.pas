{*
 * Tyler Filla
 * February 18, 2019
 * CS 4500-001 :: Intro to Software Profession
 *
 * IMPORTANT NOTE: The HW2 specification did not specify a change in the input
 * file name, and I have not assumed such to be the case, so 'HW1infile.txt' is
*  still the input file.
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
 * A transcript will be produced in the file 'HW2fillaOutfile.txt'. These files
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
 * BAD INPUT FILE EXAMPLE:
 * 4
 * 4
 * 1 4
 * 2 1
 * 2 4
 * 4 3
 *
 * This is an example of a bad input file which does not specify a strongly-
 * connected digraph. As you can see, with the four arrows specified, there are
 * no arrows away from circle 3. The program will abort before gameplay.
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
 * for the total number of checkmarks drawn (visits made) overall. Minima and
 * maxima are also tracked for single circle mark counts.
 *
 * At the beginning of each game, these global variables are zeroed out. At the
 * end of each game, they are copied into a per-game record which is pushed into
 * a global array for future computations.
 *
 * After all games, these stored records are used to compute final post-gameplay
 * statistics as specified.
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
  C_FILENAME_OUT = 'HW2fillaOutfile.txt';

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

  {*
   * A gameplay stats record.
   *
   * This holds the gameplay statistics from one game round. After a round has
   * been played, a record of this type is inserted into a dynamic array, and
   * then all stats are crunched after all rounds have been played.
   *}
  TLastPlayStats = record
    { The greatest number of marks given to any circle. }
    MaxMarks: integer;

    { The smallest number of marks given to any circle. }
    MinMarks: integer;

    { The total number of marks given to all circles. }
    TotalMarks: integer;
  end;

var
  { An iteration index. }
  i: integer;

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

  { The minimum number of marks in any one circle. }
  MinSingleCircleMarks: integer;

  { Tracker for all per-game stats. }
  AllStats: array of TLastPlayStats;

  { The sum of all total marks over all games. }
  SumTotalMarks: integer;

  { The maximum total marks over all games. }
  MaxTotalMarks: integer;

  { The minimum total marks over all games. }
  MinTotalMarks: integer;

  { The absolute maximum single circle marks over all games. }
  AbsMaxSingleCircleMarks: integer;

  { The absolute minimum single circle marks over all games. }
  AbsMinSingleCircleMarks: integer;

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

      { Check range of N (from 2 to 20). }
      if (N < 2) or (N > 20) then
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
 * Reset all circles.
 *}
procedure ResetCircles;
var
  { Iteration index. }
  i: integer;
begin
  { Clear stats. }
  UniqueCirclesMarked := 0;
  TotalCircleMarks := 0;
  MaxSingleCircleMarks := 0;
  MinSingleCircleMarks := 1000000;

  { Clear all marks. }
  for i := 0 to N do
    begin
      AllCircles[i - 1].Marks := 0;
    end;

  { Set the first circle as current. }
  CurrentCircle := @AllCircles[0];
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

  { Update minimum mark count if needed. }
  if CurrentCircle^.Marks < MinSingleCircleMarks then
    MinSingleCircleMarks := CurrentCircle^.Marks;

  MyWriteLn(Format('Marked circle %d (up to %d mark(s))', [CurrentCircle^.Number, CurrentCircle^.Marks]));
end;

{*
 * Assert that the initialized board is strongly-connected. If such is the case,
 * the program continues unimpeded. If not the case, the program aborts.
 *
 * Verification is performed with an exhaustive search from the perspective of
 * each circle. For each pair of circles, the graph is searched for a path that
 * connects the two circles in the pair.
 *}
procedure AssertConnectedness;
var
  { A multi-purpose iterator index. }
  i: integer;

  { The index of the first circle. }
  a: integer;

  { The first circle of the pair. }
  CircleA: TCirclePtr;

  { The index of the second circle. }
  b: integer;

  { The second circle of the pair. }
  CircleB: TCirclePtr;

  { The set of discovered circles yet to be explored. }
  OpenSet: array of TCirclePtr;

  { A temporary circle pointer. }
  TempCircle: TCirclePtr;

label
  { A label to which jumping scans the next circle. }
  NextCircle;

begin
  MyWriteLn('Verifying connectedness of the board');

  { Choose pairs of circles. }
  for a := 1 to N do
    for b := 1 to N do
      begin
        { Get initialized for the search. }
        CircleA := @AllCircles[a - 1];
        CircleB := @AllCircles[b - 1];
        SetLength(OpenSet, 0);
        TempCircle := nil;

        MyWriteLn(Format('-> Looking for a path from circle %d to circle %d', [b, a]));

        { Ignore trivial cases where both circles are the same. }
        if CircleA = CircleB then
          begin
            MyWriteLn('  -> Trivial case');
            continue;
          end;

        { Add circle B to the open set. This kicks off the process. }
        SetLength(OpenSet, Length(OpenSet) + 1);
        OpenSet[High(OpenSet)] := CircleB;

        { Try to find a path from circle B back to circle A. If we cannot, then
          the system is not strongly-connected. }
        while true do
          begin
            { Scan the open set for circle A. If we find circle A in the open
              set, then we have discovered a path from circle B to circle A. }
            for i := 1 to Length(OpenSet) do
              begin
                if OpenSet[i - 1] = CircleA then
                  begin
                    MyWriteLn(Format('  -> Found with %d nonterminal(s) remaining', [Length(OpenSet)]));
                    goto NextCircle;
                  end;
              end;

            { We bail out quickly when we discover paths. So, it follows that
              if, at any point, the open set is empty, then there is no
              connection (and, hence, the system is not strongly-connected). }
            if Length(OpenSet) = 0 then
              begin
                MyWriteln('  -> NOT FOUND');
                MyWriteln('');
                MyWriteLn(Format('FAIL: No path from circle %d to circle %d!', [b, a]));
                MyWriteLn('The configured graph is not strongly-connected! Bailing out...');
                MyWriteln('');
                MyWriteLn('Please correct your input file to describe a strongly-connected digraph.');
                Halt;
              end;

            { Pop the next circle from the array. }
            TempCircle := OpenSet[Low(OpenSet)];
            for i := 1 to Length(OpenSet) - 1 do
              OpenSet[i - 1] := OpenSet[i];
            SetLength(OpenSet, Length(OpenSet) - 1);

            { Add all newly-reachable circles to open set. }
            for i := 1 to Length(TempCircle^.Arrows) do
              begin
                SetLength(OpenSet, Length(OpenSet) + 1);
                OpenSet[High(OpenSet)] := TempCircle^.Arrows[i - 1];
              end;
          end;
      NextCircle:
      end;

  MyWriteLn('This board is a strongly-connected digraph.');
end;

{*
 * Carry out the game.
 *}
function PlayGame: TLastPlayStats;
var
  { The number of arrows from the current circle. This varies over gameplay. }
  NumArrows: integer;

  { The last circle we were in. }
  LastCircle: TCirclePtr;

  { The last randomly-chosen arrow index. }
  ChosenArrow: integer;

  { The stats of the last play. }
  PlayStats: TLastPlayStats;
begin
  { Reset all circles. }
  ResetCircles;

  MyWriteLn('Gameplay is about to begin.');
  MyWriteLn('');

  { Mark the first circle as current. }
  UniqueCirclesMarked := 1;
  MarkCurrentCircle();

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
          Halt;
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

  MyWriteLn('');
  MyWriteLn('~~~ LAST RUN STATISTICS ~~~');

  { Record gameplay stats for single round. }
  PlayStats.MaxMarks := MaxSingleCircleMarks;
  PlayStats.MinMarks := MinSingleCircleMarks;
  PlayStats.TotalMarks := TotalCircleMarks;

  MyWriteLn(Format('I. There were N=%d circles in play. This was prescribed by the input file.', [N]));
  MyWriteLn(Format('II. There were K=%d arrow(s) in play. This was prescribed by the input file.', [K]));
  MyWriteLn(Format('III. In total, %d marks were distributed across all circles. Some circles may have received many marks depending on the graph construction.', [TotalCircleMarks]));
  MyWriteLn(Format('IV. On average, each circle received %f marks.', [TotalCircleMarks / N]));
  MyWriteLn(Format('V. In any one circle, the maxmimum number of marks was %d. All circles received at most this many marks during gameplay.', [MaxSingleCircleMarks]));

  PlayGame := PlayStats;
end;

{ Program entry point. }
begin
  { Array initializations. }
  SetLength(AllCircles, 0);
  SetLength(AllStats, 0);

  { The output file. This will be used to produce a transcript of the game. }
  AssignFile(OutputFile, C_FILENAME_OUT);

  { Try to open the output file for write (creating it if needed). }
  try
    Rewrite(OutputFile);
  except
    on E: EInOutError do
      begin
        WriteLn(Format('Failed to open output file for write: %s', [E.Message]));
        Halt;
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
        Halt;
      end;
  end;

  { Assert that the board is strongly-connected. This procedure is not
    guaranteed to return given the chance the input is not strongly-connected. }
  AssertConnectedness;

  { Seed a new random sequence. }
  Randomize;

  { Play the game ten times. }
  for i := 1 to 10 do
    begin
      SetLength(AllStats, Length(AllStats) + 1);
      AllStats[High(AllStats)] := PlayGame;
    end;

  MyWriteLn('');
  MyWriteLn('Gameplay is complete!');

  MyWriteLn('');
  MyWriteLn('~~~~~~ OVERALL STATISTICS ~~~~~~');

  SumTotalMarks := 0;
  MaxTotalMarks := 0;
  MinTotalMarks := 1000000;
  AbsMaxSingleCircleMarks := 0;
  AbsMinSingleCircleMarks := 1000000;

  { Accumulate stats for all games. }
  for i := 1 to 10 do
    begin
      { Sum all totals. }
      SumTotalMarks := SumTotalMarks + AllStats[i - 1].TotalMarks;

      { Accumulate maximum total mark count. }
      if AllStats[i - 1].TotalMarks > MaxTotalMarks then
        MaxTotalMarks := AllStats[i - 1].TotalMarks;

      { Accumulate minimum total mark count. }
      if AllStats[i - 1].TotalMarks < MinTotalMarks then
        MinTotalMarks := AllStats[i - 1].TotalMarks;

      { Accumulate absolute maximum single circle mark count. }
      if AllStats[i - 1].MaxMarks > AbsMaxSingleCircleMarks then
        AbsMaxSingleCircleMarks := AllStats[i - 1].MaxMarks;

      { Accumulate absolute minimum single circle mark count. }
      if AllStats[i - 1].MinMarks < AbsMinSingleCircleMarks then
        AbsMinSingleCircleMarks := AllStats[i - 1].MinMarks;
    end;

  MyWriteLn(Format('* On average, a total number of %f marks were distributed per game.', [SumTotalMarks/10]));
  MyWriteLn(Format('* The lowest total number of marks given to a single circle is %d.', [MinTotalMarks]));
  MyWriteLn(Format('* The highest total number of marks given to a single circle is %d.', [MaxTotalMarks]));
  MyWriteLn(Format('* On average, each circle received %f marks.', [SumTotalMarks/10/N]));
  MyWriteLn(Format('* The lowest number of marks given to a single circle is %d.', [AbsMinSingleCircleMarks]));
  MyWriteLn(Format('* The highest number of marks given to a single circle is %d.', [AbsMaxSingleCircleMarks]));

  { Close the output file. }
  CloseFile(OutputFile);
end.
