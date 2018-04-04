public class ArrayList {

    final int INIT_SIZE = 20;
    final int RESIZE_SIZE = 10;

    private int[] vector;
    private int vector_capacity;
    private int vector_size;
    private int head;

    public static void main(String[] args) {
      ArrayList vec = new ArrayList();
      System.out.println(vec.size());
      System.out.println(vec.capacity());
      System.out.println(vec.get_at(3));
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
      if (newSize > this.vector_capacity) {
        int[] new_vector = new int[newSize];
        for (int i = 0; i < this.vector.size(); i++) {
          new_vector[i] = this.vector[i];
        }
        this.vector = new_vector;
        this.vector_capacity = newSize;
      }
    }

    // Add an element to the back of the ArrayList.
    public void push_back(int element) {
      this.head += 1;
      if (this.head >= this.vector_capacity) {
        int new_capacity = this.vector_capacity + RESIZE_SIZE;
        this.reserve(new_capacity);
      }
      this.vector[this.head] = element;
      this.vector_size += 1;
    }

    // Remove the last element from the ArrayList.
    public void pop_back() {
      this.vector[this.head] = 0;
      this.head -= 1;
      this.size -= 1;
    }

    // Return the value at the given position.
    public int get_at(int index) {
      return this.vector[index];
    }

    // Set the value at the given position.
    public void set_at(int index, int element) {
      if (index >= (this.vector_capacity - 1)) {
        int capacity_diff = (index - (this.vector_capacity - 1)) + 1;
        int resize_factor = (capacity_diff / RESIZE_SIZE) + 1;
        int new_capacity = this.vector_capacity + (resize_factor * RESITE_SIZE);
        this.reserve(new_capacity);
      }
      this.vector[index] = element;
      this.size += 1;
      this.head = index;
    }

    // Insert the element at the given index. 
    // The elements after that position are shifted towards the back.
    public void insert(int index, int element) {
     
    }

    // Erase a block of elements of size length, starting at index.
    // The elements following this block are shifted towards the front.
    public void erase(int index, int length) {

    }
}
