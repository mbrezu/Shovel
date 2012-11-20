using System;

namespace Shovel
{
	public class Stack
	{
		ShovelValue[] storage;
		int length;

		public Stack ()
		{
			this.storage = new ShovelValue[1024];
		}

		public ShovelValue Top ()
		{
			return this.storage [this.length - 1];
		}

        public ShovelValue UnderTop (int i)
        {
            return this.storage [this.length - i - 1];
        }

        public ShovelValue UnderTopOne ()
        {
            return this.storage [this.length - 2];
        }

        public void UnderPopOneAndCopyTop()
        {
            this.storage[this.length - 2] = this.storage[this.length - 1];
            this.length --;
        }

        public void UnderPopAndCopyTop (int i)
        {
            this.storage[this.length - i - 1] = this.storage[this.length - 1];
            this.length -= i;
        }

		public ShovelValue PopTop ()
		{
			var result = Top ();
			this.length --;
			return result;
		}

		public void Pop()
		{
			this.length --;
		}

		public void Push (ShovelValue value)
		{
			if (this.length < this.storage.Length) {
				this.storage [this.length] = value;
				this.length++;
			} else {
				var newStorage = new ShovelValue[this.storage.Length * 2];
				Array.Copy (this.storage, this.storage, this.length);
				this.storage = newStorage;
				this.Push(value);
			}
		}

		public void SetTop(ShovelValue value) 
		{
			this.storage[this.length - 1] = value;
		}

		public int Count {
			get { return this.length;}
		}

		public ShovelValue[] Storage {
			get { return this.storage; }
		}

		public void RemoveRange(int position, int rangeLength)
		{
			Array.Copy(this.storage, position + rangeLength, this.storage, position, rangeLength);
			this.length -= rangeLength;
		}

		public void PopMany(int n)
		{
			this.length -= n;
		}

		public void GetTopRange(int n, out ShovelValue[] array, out int start) 
		{
			array = this.storage;
			start = this.length - n;
		}

        public bool TopIsReturnAddress ()
        {
            return this.storage[this.length - 1].Kind == ShovelValue.Kinds.ReturnAddress;
        }

	}
}

