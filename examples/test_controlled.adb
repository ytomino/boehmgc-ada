with Ada.Finalization;
with Ada.Text_IO;
with System.Address_Image;
with System.Storage_Elements;
with GC.Pools;
procedure test_controlled is
	package Dummies is
		type Dummy is new Ada.Finalization.Controlled with null record;
		overriding procedure Initialize (Object : in out Dummy);
		overriding procedure Finalize (Object : in out Dummy);
	end Dummies;
	package body Dummies is
		overriding procedure Initialize (Object : in out Dummy) is
		begin
			Ada.Text_IO.Put_Line (
				"Initialize (" & System.Address_Image (Object'Address) & ")");
		end Initialize;
		overriding procedure Finalize (Object : in out Dummy) is
		begin
			Ada.Text_IO.Put_Line (
				"Finalize (" & System.Address_Image (Object'Address) & ")");
		end Finalize;
	end Dummies;
	type Dummy_Access is access Dummies.Dummy;
	for Dummy_Access'Storage_Pool use GC.Pools.Controlled_Pool;
begin
	loop
		for I in 1 .. 1000 loop
			declare
				Gomi : constant Dummy_Access := new Dummies.Dummy;
				pragma Unreferenced (Gomi);
			begin
				null;
			end;
		end loop;
		Ada.Text_IO.Put_Line (
			System.Storage_Elements.Storage_Count'Image (GC.Heap_Size));
	end loop;
end test_controlled;
