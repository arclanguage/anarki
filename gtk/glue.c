#include <gtk/gtk.h>

/*
  functions to obtain sizes of various gtk structures
*/
unsigned long get_tree_iter_size()
{
  return sizeof(GtkTreeIter);
}

unsigned long get_gvalue_size()
{
  return sizeof(GValue);
}

unsigned long get_text_iter_size()
{
  return sizeof(GtkTextIter);
}
