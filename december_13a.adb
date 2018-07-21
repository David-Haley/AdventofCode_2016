with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with December_13_Common; use December_13_Common;

procedure December_13a is

   Visited_Count : Natural := 0;
   Maximum_Steps : constant Natural := 50;
   Visited : Bread_Crumbs :=  (others => (others => False));

   procedure Search (X, Y : in Coordinates; Path_Length : in Natural;
                          Bread_Crumb : in out Bread_Crumbs) is

   begin -- Search
      if Path_Length <= Maximum_Steps and not Bread_Crumb (X, Y) then
         Bread_Crumb (X, Y) := True;
         Visited (X, Y) := True;
         if X > 0 and then Is_Open_Space (X - 1, Y) then
            Search (X - 1, Y, Path_Length + 1, Bread_Crumb);
         end if; --  X > 0 and then Is_Open_Space (X - 1, Y)
         if Y > 0 and then  Is_Open_Space (X, Y - 1) then
            Search (X, Y - 1, Path_Length + 1, Bread_Crumb);
         end if; --  Y > 0 and then  Is_Open_Space (X, Y - 1)
         if Is_Open_Space (X + 1, Y) then
            Search (X + 1, Y, Path_Length + 1, Bread_Crumb);
         end if; --  Is_Open_Space (X + 1, Y)
         if Is_Open_Space (X, Y + 1) then
            Search (X, Y + 1, Path_Length + 1, Bread_Crumb);
         end if; --  Is_Open_Space (X, Y + 1)
         Bread_Crumb (X, Y) := False;
      end if; -- Path_Length <= Maximum_Steps and not Bread_Crumb (X, Y)
   end Search;

begin -- December_13a
   Search (1, 1, 0, Bread_Crumb);
   for X in Coordinates loop
      for Y in Coordinates loop
         If Visited (X, Y) then
            Visited_Count := Visited_Count + 1;
         end if; -- Bread_Crumb (X, Y)
      end loop; -- Y in Coordinates
   end loop; -- X in Coordinates
   Put_Line ("Locations Visited:" & Natural'Image (Visited_Count));
end December_13a;
