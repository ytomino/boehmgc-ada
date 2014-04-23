with System.Address_Image; -- debug
pragma Warnings (Off);
with System.Finalization_Masters;
pragma Warnings (On);
with C.sys.mman;
separate (GC.Pools)
package body Finalization is
	use type System.Finalization_Masters.Finalize_Address_Ptr; -- debug
	use type System.Storage_Elements.Integer_Address;
	use type C.signed_int;
	use type C.unsigned_int;
	
	package SysFM renames System.Finalization_Masters; -- short name
	
	function To_FM_Node_Ptr is new Ada.Unchecked_Conversion (
		System.Address,
		SysFM.FM_Node_Ptr);
	
	type Relative_Address is mod 2 ** 32;
	
	type jmp_Rec is record
		Code : System.Storage_Elements.Storage_Element;
		Data : Relative_Address;
	end record;
	pragma Pack (jmp_Rec);
	
	Old_Instruction : jmp_Rec;
	
	Allocating_Object : System.Address := System.Null_Address;
	
	procedure Custom_Set_Finalize_Address_Unprotected (
		Master : in out SysFM.Finalization_Master;
		Fin_Addr_Ptr : in SysFM.Finalize_Address_Ptr);
	procedure Custom_Set_Finalize_Address_Unprotected (
		Master : in out SysFM.Finalization_Master;
		Fin_Addr_Ptr : in SysFM.Finalize_Address_Ptr) is
	begin
		if Allocating_Object = System.Null_Address then
			-- un-hook
			declare
				jmp : jmp_Rec;
				for jmp'Address use SysFM.Set_Finalize_Address_Unprotected'Code_Address;
			begin
				jmp := Old_Instruction;
			end;
			-- call original
			SysFM.Set_Finalize_Address_Unprotected (Master, Fin_Addr_Ptr);
			-- re-hook
			Initialize;
		else
			pragma Assert (Fin_Addr_Ptr /= null);
			SysFM.Set_Is_Heterogeneous (Master);
			SysFM.Set_Heterogeneous_Finalize_Address_Unprotected (
				Allocating_Object,
				Fin_Addr_Ptr);
			Allocating_Object := System.Null_Address;
		end if;
	end Custom_Set_Finalize_Address_Unprotected;
	
	-- implementation
	
	procedure Initialize is
		-- hook Set_Heterogeneous_Finalize_Address_Unprotected
		Page_Size : constant := 4096;
		Old_Target : constant System.Address :=
			SysFM.Set_Finalize_Address_Unprotected'Code_Address;
		New_Target : constant System.Address :=
			Custom_Set_Finalize_Address_Unprotected'Code_Address;
		jmp_Rec_Size : constant System.Storage_Elements.Storage_Count :=
			jmp_Rec'Size / Standard'Storage_Unit;
		jmp : jmp_Rec;
		for jmp'Address use Old_Target;
		Result : C.signed_int;
	begin
		-- make code memory as writable
		Result := C.sys.mman.mprotect (
			C.void_ptr (System.Storage_Elements.To_Address (
				System.Storage_Elements.To_Integer (Old_Target) and not (Page_Size - 1))),
			C.size_t (Page_Size),
			C.signed_int (C.unsigned_int'(
				C.sys.mman.PROT_READ or C.sys.mman.PROT_WRITE or C.sys.mman.PROT_EXEC)));
		pragma Assert (Result = 0);
		-- save
		Old_Instruction := jmp;
		-- make jmp instruction
		jmp.Code := 16#e9#;
		jmp.Data := Relative_Address'Mod (
			System.Storage_Elements.To_Integer (New_Target)
			- System.Storage_Elements.To_Integer (Old_Target)
			- System.Storage_Elements.Integer_Address (jmp_Rec_Size));
	end Initialize;
	
	function Controlled_Size_In_Storage_Elements
		return System.Storage_Elements.Storage_Count is
	begin
		return (SysFM.FM_Node'Size + System.Word_Size - 1) / System.Word_Size;
	end Controlled_Size_In_Storage_Elements;
	
	procedure After_Allocation (Storage_Address : in System.Address) is
	begin
		Allocating_Object := Storage_Address + SysFM.Header_Offset;
	end After_Allocation;
	
	procedure Finalize_Controlled (obj : C.void_ptr; cd : C.void_ptr) is
		pragma Unreferenced (cd);
		Obj_Addr : constant System.Address :=
			System.Address (obj) + SysFM.Header_Offset;
		Do_Finalize : constant SysFM.Finalize_Address_Ptr :=
			SysFM.Finalize_Address_Unprotected (Obj_Addr);
	begin
		SysFM.Detach (To_FM_Node_Ptr (System.Address (obj)));
		pragma Assert (Do_Finalize /= null,
			"Finalize_Address_Unprotected ("
			& System.Address_Image (Obj_Addr)
			& ") = null");
		Do_Finalize.all (Obj_Addr);
		SysFM.Delete_Finalize_Address_Unprotected (Obj_Addr);
	end Finalize_Controlled;
	
end Finalization;
