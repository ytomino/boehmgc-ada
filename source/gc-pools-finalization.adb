pragma Warnings (Off);
with System.Finalization_Implementation;
pragma Warnings (On);
separate (GC.Pools)
package body Finalization is
	
	--  same as System.Finalization_Implementation.To_Finalizable_Ptr
	function To_Finalizable_Ptr is new Ada.Unchecked_Conversion (
		System.Address,
		System.Finalization_Root.Finalizable_Ptr);
	
	-- implementation
	
	procedure Initialize is
	begin
		null;
	end Initialize;
	
	function Controlled_Size_In_Storage_Elements
		return System.Storage_Elements.Storage_Count is
	begin
		return (System.Finalization_Root.Root_Controlled'Size + System.Word_Size - 1)
			/ System.Word_Size;
	end Controlled_Size_In_Storage_Elements;
	
	procedure After_Allocation (Storage_Address : in System.Address) is
	begin
		null;
	end After_Allocation;
	
	procedure Finalize_Controlled (obj : C.void_ptr; cd : C.void_ptr) is
		pragma Unreferenced (cd);
	begin
		System.Finalization_Implementation.Finalize_One (
			To_Finalizable_Ptr (System.Address (obj)).all);
	end Finalize_Controlled;
	
end Finalization;
