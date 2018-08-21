with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Bounded_Synchronized_Queues;

procedure December_22 is

   Input_File : File_Type;

   File_System_Text : constant String := "/dev/grid/node";
   X_Prefix : constant String := "-x";
   Y_Prefix : constant String := "-y";

   X_Min, Y_Min : Natural;
   X_Max, Y_Max : Natural;

   procedure Skip (Text : in Unbounded_String;
                   To_Skip : in String;
                   Start_At : in out Positive) is
      Last : Natural;

   begin -- Skip
      Last := Index (Text, To_Skip, Start_At, Forward);
      Assert (Last /= 0, To_Skip & " not found at line:" &
                Positive_Count'Image (Line (Input_File)));
      Start_At := Last + To_Skip'Length + 1;
   end Skip;

   function Coordinate (Text : in Unbounded_String;
                        Prefix : in String;
                        Start_At : in out Positive) return Natural is

      First : Positive;
      Last : Natural;

   begin -- Coordinate
      Last := Index (Text, Prefix, Start_At, Forward);
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Assert (Last /= 0, "Number at line:" &
                   Positive_Count'Image (Line (Input_File)));
         Start_At := Last + 1;
         return Natural'Value (Slice (Text, First, Last));
   end Coordinate;

   procedure Find_Limits (Input_File : in File_Type;
                          X_Min, Y_Min, X_Max, Y_Max : out Natural) is

      Start_At : Positive;
      Text : Unbounded_String;
      X, Y : Natural;

   begin -- Find_Limits
      X_Min := Natural'Last;
      Y_Min := Natural'Last;
      X_Max := Natural'First;
      Y_Max := Natural'First;
      Start_At := Positive'First;
      while not End_Of_File (Input_File) loop
         Get_line (Input_File, Text);
         exit when Index (Text, File_System_Text, Start_At, Forward) /= 0;
      end loop; -- not End_Of_File (Input_File)
      loop -- process body of file
         Start_At := Positive'First;
         Skip (Text, File_System_Text, Start_At);
         X := Coordinate (Text, X_Prefix, Start_At);
         Y := Coordinate (Text, Y_Prefix, Start_At);
         if X > X_Max then
            X_Max := X;
         end if; -- X > X_Max
         if X < X_Min then
            X_Min := X;
         end if; -- X < X_Min
         if Y > Y_Max then
            Y_Max := Y;
         end if; -- Y > Y_Max
         if Y < Y_Min then
            Y_Min := Y;
         end if; -- Y < Y_Min
         exit when End_Of_File (Input_File);
         Get_line (Input_File, Text);
      end loop; -- process body of file
   end Find_Limits;

   procedure Process (Input_File : in File_Type;
                      X_Min, Y_Min, X_Max, Y_Max : in Natural) is

      subtype X_Coordinates is Natural range X_Min .. X_Max;
      subtype Y_Coordinates is Natural range Y_Min .. Y_Max;

      Exit_X : constant X_Coordinates := X_Coordinates'First;
      Exit_Y : constant Y_Coordinates := Y_Coordinates'First;

      type Nodes is record
         Size, Used, Available : Natural;
      end record; -- Nodes

      type Node_Grids is array (X_Coordinates, Y_Coordinates) of Nodes;

      Node_Grid : Node_Grids;

      procedure Read_Nodes (Input_File : in File_Type;
                            Node_Grid : out Node_Grids;
                            Empty_X : out X_Coordinates;
                            Empty_Y : out Y_Coordinates) is

         Start_At : Positive;
         Text : Unbounded_String;
         X, Y : Natural;

         function Node_Value (Text : in Unbounded_String;
                              Start_At : in out Positive)
                           return Natural is

            First : Positive;
            Last : Natural;

         begin -- Node_Value
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            Assert (Last /= 0, "Bad value at line:" &
                      Positive_Count'Image (Line (Input_File)));
            Start_At := Last + 1;
            Assert (Element (Text, Start_At) = 'T', "'T' not found");
            Start_At := Start_At + 1;
            return Natural'Value (Slice (Text, First, Last));
         end Node_Value;

      begin -- Read_Nodes
         Start_At := Positive'First;
         while not End_Of_File (Input_File) loop
            Get_line (Input_File, Text);
            exit when Index (Text, File_System_Text, Start_At, Forward) /= 0;
         end loop; -- not End_Of_File (Input_File)
         loop -- process body of file
            Start_At := Positive'First;
            Skip (Text, File_System_Text, Start_At);
            X := Coordinate (Text, X_Prefix, Start_At);
            Y := Coordinate (Text, Y_Prefix, Start_At);
            Node_Grid (X, Y).Size := Node_Value (Text, Start_At);
            Node_Grid (X, Y).Used := Node_Value (Text, Start_At);
            if Node_Grid (X, Y).Used = 0 then
               Empty_X := X;
               Empty_Y := Y;
            end if; -- Node_Grid (X, Y).Used = 0 then
            Node_Grid (X, Y).Available := Node_Value (Text, Start_At);
            exit when End_Of_File (Input_File);
            Get_line (Input_File, Text);
         end loop; -- process body of file
      end Read_Nodes;

      function Viable_Pairs (Node_Grid : in Node_Grids) return Natural is

         Pair_Count : Natural := 0;

      begin -- Viable_Pairs
         for A_X in X_Coordinates loop
            for A_Y in Y_Coordinates loop
               for B_X in X_Coordinates loop
                  for B_Y in Y_Coordinates loop
                     if (A_X /= B_X or A_Y /= B_Y) and then
                       Node_Grid (A_X, A_Y).Used /= 0 and then
                       Node_Grid (A_X, A_Y).Used <=
                       Node_Grid (B_X, B_Y).Available then
                        Pair_Count := Pair_Count + 1;
                     end if; -- pair
                  end loop; -- B_Y in Y_Coordinates
               end loop; -- B_X in X_Coordinates
            end loop; -- A_Y in Y_Coordinates
         end loop; -- A_X in X_Coordinates
         return Pair_Count;
      end Viable_Pairs;

      procedure Visualise (Node_Grid : in Node_Grids;
                           Target_X : in X_Coordinates;
                           Target_Y : in Y_Coordinates;
                           Empty_X : in X_Coordinates;
                           Empty_Y : in Y_Coordinates) is

      begin -- Visualise
         for Y in Y_Coordinates loop
            for X in X_Coordinates loop
               if X = Empty_X and Y = Empty_Y then
                  Put ("  E  ");
               elsif X = Target_X and Y = Target_Y then
                  Put ("  T  ");
               else
                  if X > X_Coordinates'First and then
                    Node_Grid (X - 1, Y).Size >= Node_Grid (X, Y).Used then
                     Put (" L");
                  else
                     Put (" x");
                  end if; -- move left possible
                  if X < X_Coordinates'Last and then
                    Node_Grid (X + 1, Y).Size >= Node_Grid (X, Y).Used then
                     Put ('R');
                  else
                     Put ('x');
                  end if; -- move right possible
                  if Y > Y_Coordinates'First and then
                    Node_Grid (X, Y - 1).Size >= Node_Grid (X, Y).Used then
                     Put ('U');
                  else
                     Put ('x');
                  end if; -- move up possible
                  if Y < Y_Coordinates'Last and then
                    Node_Grid (X, Y + 1).Size >= Node_Grid (X, Y).Used then
                     Put ('D');
                  else
                     Put ('x');
                  end if; -- move down possible
               end if; -- Node_Grid (X, Y).Used = 0
            end loop; -- X in X_Coordinates
            New_line;
         end loop; -- Y in Y_Coordinates
      end Visualise;

      function Part_Two (Target_X : in X_Coordinates;
                         Target_Y : in Y_Coordinates;
                         Empty_X : in X_Coordinates;
                         Empty_Y : in Y_Coordinates) return Natural is

         function Manhattan (X1 : in X_Coordinates;
                             Y1 : in Y_Coordinates;
                             X2 : in X_Coordinates;
                             Y2 : in Y_Coordinates) return Natural is
         begin -- Manhattan
            return abs (X2 - X1) + abs (Y2 - Y1);
         end Manhattan;

         Wall_End_X : constant X_Coordinates := 1;
         Wall_End_Y : constant Y_Coordinates := 2;

      begin -- Part_Two
         return Manhattan (Wall_End_X, Wall_End_Y, Empty_X, Empty_Y) +
           Manhattan (Target_X - 1, Target_Y, Wall_End_X, Wall_End_Y) +
           (Target_X - 1) * 5 + 1;
      end Part_Two;

      Target_X : X_Coordinates := X_Coordinates'Last;
      Target_Y : Y_Coordinates := Y_Coordinates'First;
      Empty_X : X_Coordinates;
      Empty_Y : Y_Coordinates;

   begin -- Process
      Read_Nodes (Input_File, Node_Grid, Empty_X, Empty_Y);
      Put_Line ("Pairs:" & Natural'Image (Viable_Pairs (Node_Grid)));
      Visualise (Node_Grid, Target_X, Target_Y, Empty_X, Empty_Y);
      Put_Line ("Part_Two" & Natural'Image (Part_Two (Target_X, Target_Y,
                Empty_X, Empty_Y)));
                end Process;

begin -- December_22
   Open (Input_File, In_File, "20161222.txt");
   Find_Limits (Input_File, X_Min, Y_Min, X_Max, Y_Max);
   Reset (Input_File);
   Process (Input_File, X_Min, Y_Min, X_Max, Y_Max);
   Close (Input_File);
end December_22;
