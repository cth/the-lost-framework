member() {
        target=$1
        shift

        for i in $@
        do
                if [ x$i = "x$target" ]; then
                        echo "1"
                        return
                fi
        done
        echo "0"
}


member 1 4 3 2 3 1 4 5 
member 1 4 3 2 3 4 5 
