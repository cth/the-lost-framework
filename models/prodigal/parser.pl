% A DCG parser for prodigal result files

cds_entry(Left,Right,Strand,Frame,Extra) -->
        cds_first_line(Left,Right),
        newline,
        cds_second_line(Extra),
        newline.

cds_first_line(Left,Right,'+',Frame) -->
        spaces,
        cds,
        left_right(Left,Right),
        spaces.

cds_first_line(Left,Right,'-',Frame) -->
        spaces,
        cds,
        complement_left_right(Left,Right),
        spaces.

left_right(Left,Right) -->
       integer(Left),
       dcg_codes('..'),
       integet(Right).

% special case in E.coli first predicted gene..
left_right(Left,Right) -->
       dcg_codes('<'),
       integer(Left),
       dcg_codes('..'),
       integet(Right).

complement_left_right(Left,Right) -->
        dcg_codes('complement('),
        left_right(Left,Right),
        dcg_codes(')').

       
dcg_codes('..')

dcg_codes(X) -->
        { atom_codes(X,Y) },
        Y.

cds -->   
        { atom_codes('CDS',Codes) },
        Codes.


spaces --> [].
spaces --> space, spaces.

space --> [ 9 ]. % tab character
space --> [ 32 ]. % normal space character
         
