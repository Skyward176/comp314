let reverse_string (s: string): string =
    for i = (String.length s) - 1 downto 0 do
        print_char(s.[i])
    done;
    print_char '\n';
    s;; 

reverse_string("hello");;