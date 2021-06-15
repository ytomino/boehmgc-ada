with Ada.Text_IO;
with System.Storage_Elements;
with GC.Pools;
procedure try_gc is
	type Integer_Access is access Integer;
	for Integer_Access'Storage_Pool use GC.Pools.Pool;
begin
	loop
		for I in 1 .. 1000 loop
			declare
				Gomi : constant Integer_Access := new Integer'(100);
			begin
				Gomi.all := 200;
			end;
		end loop;
		Ada.Text_IO.Put_Line (
			System.Storage_Elements.Storage_Count'Image (GC.Heap_Size));
	end loop;
end try_gc;
