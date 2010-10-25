with System;
with System.Storage_Pools;
package GC.Pools is
	
	type GC_Storage_Pool is new System.Storage_Pools.Root_Storage_Pool
		with null record;
	
	overriding procedure Allocate (
		Pool : in out GC_Storage_Pool;
		Storage_Address : out System.Address;
		Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
		Alignment : in System.Storage_Elements.Storage_Count);
	
	overriding procedure Deallocate (
		Pool : in out GC_Storage_Pool;
		Storage_Address : in System.Address;
		Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
		Alignment : in System.Storage_Elements.Storage_Count);
	
	overriding function Storage_Size (Pool : GC_Storage_Pool)
		return System.Storage_Elements.Storage_Count;
	
	Pool : GC_Storage_Pool;
	
	type GC_Controlled_Storage_Pool is new GC_Storage_Pool with null record;
	
	overriding procedure Allocate (
		Pool : in out GC_Controlled_Storage_Pool;
		Storage_Address : out System.Address;
		Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
		Alignment : in System.Storage_Elements.Storage_Count);
	
	overriding procedure Deallocate (
		Pool : in out GC_Controlled_Storage_Pool;
		Storage_Address : in System.Address;
		Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
		Alignment : in System.Storage_Elements.Storage_Count);
	
	Controlled_Pool : GC_Controlled_Storage_Pool;
	
end GC.Pools;
