with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with December_13_Common; use December_13_Common;

procedure December_13 is

   Target_X : Coordinates := 31;
   Target_Y : Coordinates := 39;
   Input_File : File_Type;
   Saved_Path_Length : Natural := Natural'Last;

   procedure Find_Target (X, Y : in Coordinates; Path_Length : in Natural;
                         Bread_Crumb : in out Bread_Crumbs) is

      -- uses golbal values Target_X, Target_Y and Saved_Path_Length

   begin -- Find_Target
      if not Bread_Crumb (X, Y) then
         Bread_Crumb (X, Y) := True;
         if X = Target_X and Y = Target_Y then
            -- reached target
            if Path_Length < Saved_Path_Length then
               Saved_Path_Length := Path_Length;
            end if; -- Path_Length < Saved_Path_Lenhth
         else
            if X > 0 and then Is_Open_Space (X - 1, Y) then
               Find_Target (X - 1, Y, Path_Length + 1, Bread_Crumb);
            end if; --  X > 0 and then Is_Open_Space (X - 1, Y)
            if Y > 0 and then  Is_Open_Space (X, Y - 1) then
               Find_Target (X, Y - 1, Path_Length + 1, Bread_Crumb);
            end if; --  Y > 0 and then  Is_Open_Space (X, Y - 1)
            if Is_Open_Space (X + 1, Y) then
               Find_Target (X + 1, Y, Path_Length + 1, Bread_Crumb);
            end if; --  Is_Open_Space (X + 1, Y)
            if Is_Open_Space (X, Y + 1) then
               Find_Target (X, Y + 1, Path_Length + 1, Bread_Crumb);
            end if; --  Is_Open_Space (X, Y + 1)
         end if; -- X = Target_X and Y = Target_Y
         Bread_Crumb (X, Y) := False;
      end if; -- not Bread_Crumb (X, Y)
   end Find_Target;

begin -- December_13
   Find_Target (1, 1, 0, Bread_Crumb);
   Put_Line ("Shortest Path:" & Natural'Image (Saved_Path_Length));
end December_13;
