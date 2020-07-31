with Ada.Unchecked_Conversion;
pragma Warnings (Off);
with System.Finalization_Root;
pragma Warnings (On);
with C.gc.gc;
with C.gc.gc_typed;
package body GC.Pools is
	use type System.Address;
	use type System.Storage_Elements.Storage_Count;
	use type C.size_t;
	
	package Finalization is
		
		Initialized : Boolean := False;
		
		procedure Initialize;
		
		function Controlled_Size_In_Storage_Elements
			return System.Storage_Elements.Storage_Count;
		
		procedure After_Allocation (Storage_Address : in System.Address);
		
		procedure Finalize_Controlled (obj : C.void_ptr; cd : C.void_ptr)
			with Convention => C;
		
	end Finalization;
	
	package body Finalization is separate;
	
	-- implementation
	
	overriding procedure Allocate (
		Pool : in out GC_Storage_Pool;
		Storage_Address : out System.Address;
		Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
		Alignment : in System.Storage_Elements.Storage_Count)
	is
		pragma Unreferenced (Pool);
	begin
		Storage_Address :=
			System.Address (C.gc.gc.GC_malloc (C.size_t (Size_In_Storage_Elements)));
		if Storage_Address = System.Null_Address
			or else (Alignment > 1 and then Storage_Address mod Alignment /= 0)
		then
			raise Storage_Error;
		end if;
	end Allocate;
	
	overriding procedure Deallocate (
		Pool : in out GC_Storage_Pool;
		Storage_Address : in System.Address;
		Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
		Alignment : in System.Storage_Elements.Storage_Count)
	is
		pragma Unreferenced (Pool);
		pragma Unreferenced (Size_In_Storage_Elements);
		pragma Unreferenced (Alignment);
	begin
		C.gc.gc.GC_free (C.void_ptr (Storage_Address));
	end Deallocate;
	
	overriding function Storage_Size (Pool : GC_Storage_Pool)
		return System.Storage_Elements.Storage_Count
	is
		pragma Unreferenced (Pool);
	begin
		return System.Storage_Elements.Storage_Offset (C.gc.gc.GC_get_heap_size);
	end Storage_Size;
	
	overriding procedure Allocate (
		Pool : in out GC_Controlled_Storage_Pool;
		Storage_Address : out System.Address;
		Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
		Alignment : in System.Storage_Elements.Storage_Count)
	is
		pragma Unreferenced (Pool);
	begin
		if not Finalization.Initialized then
			Finalization.Initialized := True;
			Finalization.Initialize;
		end if;
		declare
			Words : constant C.size_t :=
				C.size_t (
					(Size_In_Storage_Elements * System.Storage_Unit + System.Word_Size - 1)
						/ System.Word_Size);
			Controlled_Words : constant C.size_t :=
				C.size_t (Finalization.Controlled_Size_In_Storage_Elements);
			type Bits_Array is array (1 .. Words) of Boolean;
			pragma Pack (Bits_Array);
			type Bits_Array_Access is access all Bits_Array;
			function To_GC_bitmap is
				new Ada.Unchecked_Conversion (Bits_Array_Access, C.gc.gc_typed.GC_bitmap);
			Bitmap : aliased Bits_Array;
			Desc : C.gc.gc_typed.GC_descr;
		begin
			Bitmap (1 .. Controlled_Words) := (others => False);
			Bitmap (Controlled_Words + 1 .. Words) := (others => True);
			Desc := C.gc.gc_typed.GC_make_descriptor (To_GC_bitmap (Bitmap'Access), Words);
			Storage_Address :=
				System.Address (
					C.gc.gc_typed.GC_malloc_explicitly_typed (
						C.size_t (Size_In_Storage_Elements),
						Desc));
			if Storage_Address = System.Null_Address
				or else (Alignment > 1 and then Storage_Address mod Alignment /= 0)
			then
				raise Storage_Error;
			end if;
		end;
		C.gc.gc.GC_register_finalizer_no_order (
			C.void_ptr (Storage_Address),
			Finalization.Finalize_Controlled'Access,
			C.void_ptr (System.Null_Address),
			null,
			null);
		Finalization.After_Allocation (Storage_Address);
	end Allocate;
	
	overriding procedure Deallocate (
		Pool : in out GC_Controlled_Storage_Pool;
		Storage_Address : in System.Address;
		Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
		Alignment : in System.Storage_Elements.Storage_Count)
	is
		pragma Unreferenced (Pool);
		pragma Unreferenced (Size_In_Storage_Elements);
		pragma Unreferenced (Alignment);
	begin
		C.gc.gc.GC_register_finalizer_no_order (
			C.void_ptr (Storage_Address),
			null,
			C.void_ptr (System.Null_Address),
			null,
			null);
		C.gc.gc.GC_free (C.void_ptr (Storage_Address));
	end Deallocate;
	
begin
	C.gc.gc.GC_init;
end GC.Pools;
