typedef struct map_struct
{
  struct map_struct *next;
  char *stream;
  char *network;
  char *station;
  char *channel;
  int id;           
}
Map;

