extern void lang_print(const char* s);
extern void lang_panic(const char* s);

int
main()
{
  lang_print("Hello world\n");
  lang_panic("Linux");
}
