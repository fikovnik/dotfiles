set history save on
set pagination off
set print pretty on
set auto-load safe-path /

define ninja
    shell ninja
    python gdb.execute("file " + gdb.current_progspace().filename)
    directory
end

define pp
    call (void)operator<<(std::cout, $arg0)
    call (void)std::cout.flush()
    printf "\n"
end
