using System;

namespace Ava
{
    class Sequence
    {

        internal struct ArrayInfo
        {
            internal int capacity;
            internal int length;
            internal int address;

            internal ArrayInfo(int array_ptr, int len, int cap) {
                address  = array_ptr;
                length   = len;
                capacity = cap;
            }

            internal ArrayInfo(Script vm, int array_ptr) {
                address  = array_ptr;
                capacity = vm.ReadWord(address - 2);
                length   = vm.ReadWord(address - 1);
            }
        }

        ///
        /// Push element to the end of an array, reallocate the array
        /// if it's full. If new memory is allocated the old array is
        /// freed.
        ///
        /// arg(0) : Address of the array
        /// arg(1) : Element
        ///
        /// Note that the address is passed by value, so Push returns
        /// a pointer to the allocated array. In the future, It might
        /// be better to pass a pointer to the array instead, so that
        /// the reference can be updated.
        ///
        internal static Value Push(Script vm) {
            var array   = new ArrayInfo(vm, vm.Argument(0).AsInt);
            int element = vm.Argument(1).AsInt;

            if (array.length >= array.capacity) {
                // Double array capacity
                int new_array = Heap.Alloc(vm, array.capacity * 2);

                vm.MemCopy(array.address, new_array, array.capacity);

                if (array.address >= vm.cpu.heap) {
                    // Only free array if it's in dynamic memory
                    Heap.Free(vm, array.address);
                }
                array.address = new_array;
            }

            vm.WriteWord(element, array.address + array.length);
            vm.WriteWord(array.length + 1, array.address - 1);
            return new Value(array.address);
        }

        internal static Value Pop(Script vm) {
            var array   = new ArrayInfo(vm, vm.Argument(0).AsInt);
            int element = vm.Argument(1).AsInt;

            if (array.length <= 0) {
                vm.cpu.status = Cpu.Status.BAD_MEMORY_ACCESS;
                return new Value(-1);
            }

            vm.WriteWord(array.length - 1, array.address - 1);
            return new Value(vm.ReadWord(array.address + array.length - 1));
        }

        internal static Value Clear(Script vm) {
            var array = new ArrayInfo(vm, vm.Argument(0).AsInt);

            Array.Clear(vm.memory, array.address, array.length);
            return Value.Void;
        }

        internal static Value Fill(Script vm) {
            int val = vm.Argument(1).AsInt;
            var array = new ArrayInfo(vm, vm.Argument(0).AsInt);

            for (int i = array.address; i < array.address + array.length; i++) {
                vm.WriteWord(val, i);
            }

            return Value.Void;
        }
    }
}
