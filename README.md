# BlogMD

A blog program generating navigator and pages according to markdown posts.

1. Read the blog directory, generate PostEntry.
2. According to PostEntry, generate navSplice
3. According to PostEntry, generate post templates with pandoc
4. Serve the snap

---

### Install

1. Dowload and extract the source, go to the source directory.
2. run `cabal install` or `cabal-dev install`

---

### Execute

1. Create a post directory :

        posts/
            subject1/file1.md
            subject2/file2.md


2. run `blogmd`

---

### Example

* [My Blog](http://lyntonzhang.com)

---

### TODO

1. Lens
2. User Authentication
3. Unicode for FilePath of PostEntry
    * Done. 
    * But be careful, now every markdown document must start with a level 1 head, like `#Title`, and title would would show on the navigator or archive list.
4. Recursive postNav
5. Discussion and message
6. Command arg to choose posts and templates



   
