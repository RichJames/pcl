# PCL - Practical Common Lisp
## Example programs for learning Common Lisp

These subdirectories contain code from the various chapters in Peter Seibel's **Practical Common Lisp**, which I recreated while working through the book.  The code works, although I don't claim it has been thoroughly tested.

I did all my work using SBCL as my Common Lisp.

The packaging is not identical to what he presents in the book.  He doesn't discuss QuickLisp, for example, which I used.  I still have things to learn as far as preparing lisp code for production release.

There are a few things I encountered along the way that I haven't resolved, but hope to do so in the future:  

- The Shoutcast code (in *pcl-shoutcast/*) works, but I thought the author's intent was that you would be able to modify the playlist while a client was listening to it and it would dynamically update for that client.  It doesn't appear to do that.

- The Shoutcast code throws an error when the client disconnects.  The error is SB-INT:SIMPLE-STREAM-ERROR, Broken pipe.  I tried to modify the code to catch the error, but haven't figured it out yet.

- To test the Shoutcase stream, I used both RhythmBox and VLC on Linux.  RhythmBox takes a long time for the stream to start playing. VLC plays immediately when I request the stream.  I don't know why they differ like that.

- I had to add *:element-type '(unsigned-byte 8)* to the *with-open-file* macro in the **play-current** function in pcl-shoutcast.lisp in order to get the streaming to work.  The book doesn't specify this and it isn't in his code repository either.  But I get an error that the stream is not a binary stream unless I add this :element-type specification.

- I tried both forms of **play-current**.  Both worked the same way.


The *pcl-mp3-browser* folder could be considered the master project for all of this.  It uses the code in all the other subfolders, either directly or indirectly.

I had all these subfolders located on my system under ~/quicklisp/local-projects/pcl.
