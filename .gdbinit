set history save on
set pagination off
set print pretty on
set auto-load safe-path /

define ninja
    shell ninja
    python gdb.execute("file " + gdb.current_progspace().filename)
    directory
end
