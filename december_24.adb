with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Bounded_Synchronized_Queues;

procedure December_24 is

   Input_File : File_Type;
   X_Limit, Y_Limit : Positive;

   procedure Find_Limits (X_Limit, Y_Limit : out Positive) is

      Text : Unbounded_String;

   begin -- Find_Limits
      while not End_Of_File (Input_File) loop
         Y_Limit := Positive (Line (Input_File));
         Get_Line (Input_File, Text);
         X_Limit := Length (Text);
      end loop; -- not End_Of_File (Input_File)
   end Find_Limits;

   procedure Find_Solution (Input_File : in File_Type;
                            X_Limit, Y_Limit : in Positive) is

      subtype X_Coordinates is Positive range 1 .. X_Limit;
      subtype Y_Coordinates is Positive range 1 .. Y_Limit;

      Path : constant Character := '.';
      Wall : constant Character := '#';
      subtype Duct_Characters is Character
        with Static_Predicate => Duct_Characters in Path | Wall | '0' .. '9';
      subtype Target_Characters is Character range '0' .. '9';

      type Duct_Elements is record
         Duct_Character : Duct_Characters;
         Bread_Crumb : Boolean := False;
      end record; -- Duct_Elements

      type Ducts is array (X_Coordinates, Y_Coordinates) of Duct_Elements;

      type Target_Elemets is record
         X : X_Coordinates;
         Y : Y_Coordinates;
         Exists : Boolean := False;
         Visited : Boolean := False;
      end record; -- Target_Elemets

      type Targets is array (Target_Characters) of Target_Elemets;

      type Distances is array (Target_Characters, Target_Characters)
        of Positive;

      package Positive_IO is new Ada.Text_IO.Integer_IO (Positive);
      use Positive_IO;

      type Search_Queue_Elements is record
         X : X_Coordinates;
         Y : Y_Coordinates;
         Distance : Natural;
      end record; -- Search_Queue_Elements

      package Search_Queues_Interface is new
        Ada.Containers.Synchronized_Queue_Interfaces (Search_Queue_Elements);

      package Search_Queues is new Ada.Containers.Bounded_Synchronized_Queues
        (Search_Queues_Interface,
         Count_Type (X_Coordinates'Last * Y_Coordinates'Last));
      -- The bound placed on the queue is the absolute worst case, that is,
      -- every element of Duct is being examined. A better alternative would be
      -- the count of all non Wall in Duct! Note if the queue bound is too small
      -- this will not raise an exception, Enqueue will block and the search
      -- will fail in the blocked state, that is, no progress.
      use Search_Queues;

      procedure Read_Ducts (Input_File : in File_Type;
                            Duct : out Ducts;
                            Target : out Targets) is

      begin -- Read_Ducts
         for Y in Y_Coordinates loop
            for X in X_Coordinates loop
               Get (Input_File, Duct (X, Y).Duct_Character);
               if Duct (X, Y).Duct_Character in Target_Characters then
                  Target (Duct (X, Y).Duct_Character) := (X, Y, True, False);
               end if; -- Duct (X, Y).Duct_Character in Target_Characters
            end loop; --X in X_Coordinates
            Skip_Line (Input_File);
         end loop; -- Y in Y_Coordinates
      end Read_Ducts;

      procedure Find_Target (Start, To_Find : in Target_Characters;
                             Duct : in out Ducts;
                             Target : in Targets;
                             Distance_Table : in out Distances) is

         -- Search assume that there is a full boundary of Wall characters thus
         -- ensuring that X + 1, X - 1, Y + 1 and Y - 1 will always be within
         -- the bounds of Duct

         Search_Queue : Search_Queues.Queue;
         Search_Queue_Element : Search_Queue_Elements;
         X : X_Coordinates;
         Y : Y_Coordinates;
         Distance : Natural;

      begin -- Find_Target
         Enqueue (Search_Queue, (Target (Start).X, Target (Start).Y, 0));
         Duct (Target (Start).X, Target (Start).Y).Bread_Crumb := True;
         while Current_Use (Search_Queue) > 0 loop
            Dequeue (Search_Queue, Search_Queue_Element);
            X := Search_Queue_Element.X;
            Y := Search_Queue_Element.Y;
            Distance :=  Search_Queue_Element.Distance;
            exit when Duct (X, Y).Duct_Character = To_Find;
            if Duct (X - 1, Y).Duct_Character /= Wall and then
              not Duct (X - 1, Y).Bread_Crumb then
               Enqueue (Search_Queue, (X - 1, Y, Distance + 1));
               Duct (X - 1, Y).Bread_Crumb := True;
            end if; -- search left
            if Duct (X, Y - 1).Duct_Character /= Wall and then
              not Duct (X, Y - 1).Bread_Crumb then
               Enqueue (Search_Queue, (X, Y - 1, Distance + 1));
               Duct (X, Y - 1).Bread_Crumb := True;
            end if; -- search up
            if Duct (X + 1, Y).Duct_Character /= Wall and then
              not Duct (X + 1, Y).Bread_Crumb then
               Enqueue (Search_Queue, (X + 1, Y, Distance + 1));
               Duct (X + 1, Y).Bread_Crumb := True;
            end if; -- search right
            if Duct (X, Y + 1).Duct_Character /= Wall and then
              not Duct (X, Y + 1).Bread_Crumb then
               Enqueue (Search_Queue, (X, Y + 1, Distance + 1));
               Duct (X, Y + 1).Bread_Crumb := True;
            end if; -- search down
         end loop; -- Current_Use (Search_Queue) > 0
         if Duct (X, Y).Duct_Character = To_Find then
            -- something found
            Distance_Table (Start, To_Find) := Distance;
            Distance_Table (To_Find, Start) := Distance;
         end if;
         -- clean up, reset Bread_Crumb
         for X in X_Coordinates loop
            for Y in Y_Coordinates loop
               Duct (X, Y).Bread_Crumb := False;
            end loop; -- Y in Y_Coordinates
         end loop; -- X in X_Coordinates
      end Find_Target;

      procedure Find_Tour (Current : in Target_Characters;
                           Target : in out Targets;
                           Distance : in Distances;
                           Tour_Distance : in Natural;
                           Shortest_Tour : in out Natural;
                           End_At : in out Target_Characters) is

         function All_Visited (Target : in Targets) return Boolean is

            Result : Boolean := True;

         begin -- All_Visited
            for I in Target_Characters loop
               Result := Result and
                 (Target (I).Visited or not Target (I).Exists);
            end loop; -- I in Target_Characters
            return Result;
         end All_Visited;

      begin -- Find_Tour
         Target (Current).Visited := True;
         if All_Visited (Target) then
            if Tour_Distance < Shortest_Tour then
               Shortest_Tour := Tour_Distance;
               End_At := Current;
            end if; -- Tour_Distance < Shortest_Tour
         else
            for Next in Target_Characters loop
               if Target (Next).Exists and not Target (Next).Visited then
                  Find_Tour (Next, Target, Distance,
                             Tour_Distance + Distance (Current, Next),
                             Shortest_Tour, End_At);
               end if; -- Target (Next).Exists and not Target (Next).Visited
            end loop; -- Next in Target_Characters
         end if; -- All_Visited (Target)
         Target (Current).Visited := False;
      end Find_Tour;

      Duct : Ducts;
      Target : Targets;
      Start, End_At : Target_Characters := Target_Characters'First;
      Distance : Distances := (others => (others => Positive'Last));
      Shortest_Distance : Natural := Natural'Last;

   begin -- Find_Solution
      Read_Ducts (Input_File, Duct, Target);
      for Start in Target_Characters loop
         if Target (Start).Exists then
            for To_Find in Target_Characters range
              Target_Characters'Succ (Start) ..
              Target_Characters'Last loop
               If Target (To_Find).Exists then
                  Find_Target (Start, To_Find, Duct, Target, Distance);
               end if; -- Target (To_find).Exists
            end loop; -- To_Find
         end if; --Target (Start).Exists
      end loop; -- Start in Target_Characters
      Find_Tour (Target_Characters'First, Target, Distance, 0,
                 Shortest_Distance, End_At);
      Put_Line ("Shortest Distance:" & Natural'Image (Shortest_Distance));
      Shortest_Distance := Shortest_Distance +
        Distance (Target_Characters'First, End_At);
      Put_Line ("Shortest Return:" & Natural'Image (Shortest_Distance));
   end Find_Solution;

begin -- December_24
   Open (Input_File, In_File, "20161224.txt");
   Find_Limits (X_Limit, Y_Limit);
   Reset (Input_File);
   Find_Solution (Input_File, X_Limit, Y_Limit);
   Close (Input_File);
end December_24;
