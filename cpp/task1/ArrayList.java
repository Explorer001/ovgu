public class ArrayList {

    final int INIT_SIZE = 20;
    final int RESIZE_SIZE = 10;

    private int[] vector;
    private int vector_capacity;
    private int vector_size;
    private int head;

    public static void main(String[] args) {
      ArrayList vec = new ArrayList();
      for (int i = 1; i <= 29; i++) {
        vec.push_back(i);
      }
      //vec.set_at(5,37);
      //vec.insert(7,42);

      System.out.println(vec.size());
      System.out.println(vec.capacity());
      System.out.println();
      
      vec.erase(28,5);
      System.out.println(vec.size());
      
      for (int i = 0; i < vec.capacity(); i++) {
        System.out.println(vec.get_at(i));
      }
    }

    // Return the current number of elements.
    public int size() {
      return this.vector_size;
    }

    // Return the currently available size.
    public int capacity() {
      return this.vector_capacity;
    }

    // Construct a new ArrayList with some space.
    public ArrayList() {
      this.vector = new int[INIT_SIZE];
      this.vector_capacity = INIT_SIZE;
      this.vector_size = 0;
      this.head = 0;
    }

    // Increase the capacity to hold at least newSize elements.
    // This method never decreases the size of the ArrayList!
    public void reserve(int newSize) {
      //create new array with higher capacity and copy elements
      if (newSize > this.vector_capacity) {
        int[] new_vector = new int[newSize];
        for (int i = 0; i < this.vector.length; i++) {
          new_vector[i] = this.vector[i];
        }
        this.vector = new_vector;
        this.vector_capacity = newSize;
      }
    }

    // Add an element to the back of the ArrayList.
    public void push_back(int element) {
      //resize if insert index is bigger than capacity
      if (this.head + 1 >= this.vector_capacity) {
        int new_capacity = this.vector_capacity + RESIZE_SIZE;
        this.reserve(new_capacity);
      }
      //insert element at end and update head
      this.vector[this.head] = element;
      this.vector_size += 1;
      this.head += 1;
    }

    // Remove the last element from the ArrayList.
    public void pop_back() {
      this.vector[this.head] = 0;
      this.vector_size -= 1;
      this.head -= 1;
    }

    // Return the value at the given position.
    public int get_at(int index) {
      //may trigger exceptions... just like regular arrays
      return this.vector[index];
    }

    // Set the value at the given position.
    public void set_at(int index, int element) {
      //resizes the array until index is in bounds
      if (index >= this.vector_capacity) {
        int capacity_diff = (index - (this.vector_capacity - 1)) + 1;
        int resize_factor = (capacity_diff / RESIZE_SIZE) + 1;
        int new_capacity = this.vector_capacity + (resize_factor * RESIZE_SIZE);
        this.reserve(new_capacity);
      }
      //add  element at index and update head and size if nessesary
      this.vector[index] = element;
      if (index > this.head) {
        this.vector_size += 1;
        this.head = index;
      }
    }

    // Insert the element at the given index. 
    // The elements after that position are shifted towards the back.
    public void insert(int index, int element) {
      //insert element after head - no need to shift
      if (index > this.head) {
        //resize if index out of bounds
        if (index >= (this.vector_capacity)) {
          int capacity_diff = (index - (this.vector_capacity - 1)) + 1;
          int resize_factor = (capacity_diff / RESIZE_SIZE) + 1;
          int new_capacity = this.vector_capacity + (resize_factor * RESIZE_SIZE);
          this.reserve(new_capacity);
        }
        //insert element and update head
        this.vector[index] = element;
        this.head = index;
        this.vector_size += 1;
      // insert element before head - need to shift
      } else {
        this.head += 1;
        //updates the size if new head is of bounds
        if (this.head >= this.vector_capacity) {
          int new_capacity = this.vector_capacity + RESIZE_SIZE;
          this.reserve(new_capacity);
        }
        //shifts and insert element
        for (int i = head-1; i >= index; i--) {
          this.vector[i+1] = this.vector[i];
        }
        this.vector[index] = element;
        this.vector_size += 1;
      }
    }

    // Erase a block of elements of size length, starting at index.
    // The elements following this block are shifted towards the front.
    public void erase(int index, int length) {
      //if index + block_len >= head - no shift
      if ((index + length - 1) >= this.head) {
        for (int i = index; i <= this.head; i++) {
          this.vector[i] = 0;
        }
        //update size and head
        this.vector_size -= this.head - index;
        this.head = index - 1;
      } else {
        //removes block and shift
        for (int i = index; i < (index + length-1); i++) {
          this.vector[i] = 0;
        }
        int shift_index = index;
        for (int i = (index + length - 1); i <= this.head; i++) {
          this.vector[shift_index] = this.vector[i];
          this.vector[i] = 0;
          shift_index++;
        }
        this.vector_size -= (length-1);
        this.head -= (length-1);
      }
    }
}
