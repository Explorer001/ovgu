public class ArrayList {

    final int INIT_SIZE = 20;
    final int RESIZE_SIZE = 10;

    private int[] vector;
    private int vector_capacity;
    private int vector_size;
    private int head;

    public static void main(String[] args) {
      ArrayList vec = new ArrayList();
      for (int i = 0; i < 15; i++) {
        vec.push_back(i);
      }
      for (int i = 20; i < 23; i++) {
        vec.insert(3,i);
      }
      vec.erase(3,3);
      vec.push_back(15);
      vec.erase(14,5);
      System.out.println(vec.toString());
      System.out.println(vec.size());
    }

    public String toString() {
      String str = "[";
      for (int i = 0; i < this.vector_size - 1; i++) {
        str += Integer.toString(this.vector[i]) + ", ";
      }
      str += Integer.toString(this.vector[this.vector_size - 1]) + "]";
      return str;
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
      this.head = -1;
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
      this.head += 1;
      if (this.head >= this.vector_capacity) {
        int new_capacity = this.vector_capacity + RESIZE_SIZE;
        this.reserve(new_capacity);
      }
      //insert element at end and update head
      this.vector[this.head] = element;
      this.vector_size += 1;
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
      //print error if index > head
      if (index > this.head || index < 0) {
        throw new IndexOutOfBoundsException();
      }
      //add  element at index
      this.vector[index] = element;
    }

    // Insert the element at the given index. 
    // The elements after that position are shifted towards the back.
    public void insert(int index, int element) {
      //catch index > head
      if (index > this.head || index < 0) {
        throw new IndexOutOfBoundsException();
      }
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

    // Erase a block of elements of size length, starting at index.
    // The elements following this block are shifted towards the front.
    public void erase(int index, int length) {
      //removes block and shift
      int for_goal = this.head;
      int shift_element;
      int new_head = this.head;
      for (int i = index; i <= for_goal; i++) {
        shift_element = (i+length <= head)? this.vector[i+length] : 0;
        this.vector[i] = shift_element;
        new_head -= 1;
      }
      if (new_head <= (this.head - length)) {
        this.head -= length;
      } else {
        this.head = new_head;
      }
      this.vector_size = this.head + 1;
    }
}
