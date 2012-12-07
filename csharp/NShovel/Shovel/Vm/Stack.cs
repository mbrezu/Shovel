using System;

namespace Shovel
{
    public class Stack
    {
        Value[] storage;
        int length;

        public Value[] GetUsedStack ()
        {
            var result = new Value[this.length];
            Array.Copy (this.storage, result, this.length);
            return result;
        }

        public Stack ()
        {
            this.storage = new Value[1024];
        }

        public Stack (Value[] existingValues)
        {
            this.storage = new Value[existingValues.Length * 2];
            this.length = existingValues.Length;
            Array.Copy (existingValues, this.storage, this.length);
        }

        public Value Top ()
        {
            return this.storage [this.length - 1];
        }

        public Value UnderTop (int i)
        {
            return this.storage [this.length - i - 1];
        }

        public Value UnderTopOne ()
        {
            return this.storage [this.length - 2];
        }

        public void UnderPopOneAndCopyTop ()
        {
            this.storage [this.length - 2] = this.storage [this.length - 1];
            this.length --;
        }

        public void UnderPopAndCopyTop (int i)
        {
            this.storage [this.length - i - 1] = this.storage [this.length - 1];
            this.length -= i;
        }

        public Value PopTop ()
        {
            this.length --;
            return this.storage [length];
        }

        public void Pop ()
        {
            this.length --;
        }

        public void Push (Value value)
        {
            if (this.length == this.storage.Length) {
                Array.Resize (ref this.storage, this.storage.Length * 2);
            }
            this.storage [this.length] = value;
            this.length++;
        }

        public int Count {
            get { return this.length;}
        }

        public Value[] Storage {
            get { return this.storage; }
        }

        public void RemoveRange (int position, int rangeLength)
        {
            int lengthToCopy = this.length - position - rangeLength;
            Array.Copy (this.storage, position + rangeLength, this.storage, position, lengthToCopy);
            this.length -= rangeLength;
        }

        public void PopMany (int n)
        {
            this.length -= n;
        }

        public void GetTopRange (int n, out Value[] array, out int start)
        {
            array = this.storage;
            start = this.length - n;
        }

        public bool TopIsReturnAddress ()
        {
            return this.storage [this.length - 1].Kind == Value.Kinds.ReturnAddress;
        }

    }
}

