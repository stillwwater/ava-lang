namespace Ava
{
    ///
    /// Used as a container to convert between C# types and Ava cells.
    /// Values are read from, and written to the Ava stack as single cell.
    ///
    /// For dynamically sized types such as strings, a reference to the Ava
    /// machine instance is required so that the data can be read from
    /// memory. In this case, the value contains the address of the data on
    /// the heap.
    ///
    /// A value instance can also be created with Value.Void which lets the
    /// Ava machine know that this value will not be written to the stack.
    ///
    public struct Value
    {
        internal bool is_void;
        int value;
        Script parent;

        ///
        /// Convert Ava cell to a float.
        ///
        public float AsFloat {
            get { return Float32.FromInt(value); }
        }

        ///
        /// Read Ava cell as an int.
        ///
        public int AsInt {
            get { return value; }
        }

        public string AsString {
            get { return parent == null ? null : Ava.String.ReadString(parent, value); }
        }

        public Value(int v, Script vm = null) {
            value = v;
            is_void = false;
            parent = vm;
        }

        public Value(float v, Script vm = null) {
            value = Float32.ToInt(v);
            is_void = false;
            parent = vm;
        }

        public Value(string str, Script vm) {
            value = Ava.String.AllocString(vm, str);
            is_void = false;
            parent = vm;
        }

        public static Value Void {
            get { return new Value() { value = 0, is_void = true }; }
        }
    }
}
