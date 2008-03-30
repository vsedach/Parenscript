#! /usr/bin/env python

import re
import tempfile
import os, getopt, sys, shutil, random
from os.path import splitext, basename

class TxtFile:
    def __init__(self, title, author = None, style = None, includeToc = None):
        self.title = title
        self.author = author
        self.buffer = ""

    def reset(self):
        self.buffer = ""

    def __add__(self, aStr):
        self.buffer += aStr
        return self

    def tempFile(self):
        return tempfile.NamedTemporaryFile('w+', -1, '.txt', 'pbook')

    def writeFile(self, fileName = None):
        if not fileName:
            file = self.tempFile()
        elif fileName == "-":
            file = sys.stdout
        else:
            name,ext = splitext(fileName)
            if ext == "":
                ext = ".txt"
            elif ext != ".txt":
                raise "Can not write format " + ext
            file = open(name + ext, "w+")
        file.write(self.buffer)
        file.flush()
        return file

    def addEscape(self, escape, fileName, line):
        return
    def addHeading(self, level, heading, fileName, line):
        heading = re.sub("\s+", " ", heading)
        self += "++ " + heading + "\n"
    def addComment(self, comment, fileName, startLine, endLine):
        self += comment
    def addFigure(self, figureFile, figureScale, figureName, fileName, endLine):
        self += "            " + figureName + " (" + figureFile + ")" + "\n"
    def addCode(self, code, fileName, startLine, endLine):
        code = code.rstrip()
        code = re.sub("\t", "   ", code)
        self += "\n== %s (%s:%s) ================\n" % (basename(fileName), startLine, endLine)
        self += code
        self += "\n=========================================================\n\n"

class TexFile(TxtFile):
    def __init__(self, title, author = None, style = "article", includeToc = True):
        TxtFile.__init__(self, title, author, style, includeToc)
        self.style = style
        self.includeToc = includeToc
        self.bookSectioningCommands = ("chapter", "section", \
                                       "subsection", "subsubsection")
        self.articleSectioningCommands = ("section", "subsection", \
                                          "subsubsection")

    def beginning(self):
        return '\n\\documentclass[notitlepage,a4paper,makeidx]{' + self.style + '}\n' + \
               '\\usepackage{fancyvrb,color,palatino,makeidx}\n' + \
               "\\usepackage{graphicx}\n" + \
               '\\definecolor{gray}{gray}{0.6}\n' + \
               '\\title{' + TexFile.escapeString(self.title) + '}\n' +  \
               (self.author and ('\\author{' + self.author + '}\n') or '') + \
               '\\makeindex' + \
               '\\begin{document}\n\\maketitle\n' + \
               (self.includeToc and '\\tableofcontents\n' or '')
    def ending(self):
        return '\\printindex\n\\end{document}\n'
    def sectioningCommand(self, level):
        if self.style == "article":
            return self.articleSectioningCommands[min(level, len(self.articleSectioningCommands))]
        elif self.style == "book":
            return self.bookSectioningCommands[min(level, len(self.bookSectioningCommands))]

    def escapeString(aStr):
        aStr = re.sub("\\\\", "$\\\\backslash$", aStr)
        def escapeRepl(match):
            if match.group(1) != '$' or \
               not (aStr[match.start():].startswith("$\\backslash$") or \
                    aStr[:match.start()].endswith("$\\backslash")):
                return '\\' + match.group(1)
            else:
                return match.group(0)
        return re.sub("([#%&~$_^{}])", escapeRepl, aStr)
    escapeString = staticmethod(escapeString)

    def tempFile(self):
        return tempfile.NamedTemporaryFile('w+', -1, '.tex', 'pbook')
    def writeFile(self, fileName = None):
        if not fileName:
            file = self.tempFile()
        elif fileName == "-":
            return self.writeTex(sys.stdout)
        else:
            name,ext = splitext(fileName)
            if ext == "":
                ext = ".pdf"
            file = open(name + ext, "w+")
        name,ext = splitext(file.name)
        if ext == ".tex":
            return self.writeTex(file)
        elif ext == ".pdf":
            return self.writePdf(file)
        else:
            raise "Can not write format " + ext

    def writeTex(self, output):
        output.write(self.beginning())
        output.write(self.buffer)
        output.write(self.ending())
        output.flush()
        return output

    def writePdf(self, output):
        tmpfile = self.tempFile()
        self.writeTex(tmpfile)
        (dir, name) = os.path.split(tmpfile.name)
        print "cd " + dir + "; latex " + name + " && pdflatex " + name
        os.system("cd " + dir + "; latex " + name + " && pdflatex " + name)
        tmpfile.close()
        pdfname = splitext(tmpfile.name)[0] + ".pdf"
        shutil.copyfile(pdfname, output.name)
        os.remove(pdfname)
        return output

    def addEscape(self, escape, fileName, line):
        self += escape + "\n"
    def addFigure(self, figureFile, figureScale, figureName, fileName, endLine):
        self += "\\begin{figure}[htbp]\n  \\centering\n"
        self += "  \\fbox{\\includegraphics[scale=" + figureScale + "]{" + figureFile + "}}\n"
        self += "  \\caption{" + figureName + "}\n"
        self += "\\end{figure}\n"
    def addHeading(self, level, heading, fileName, line):
        heading = re.sub("\s+", " ", heading)
        self += "\n\n\\" + self.sectioningCommand(level) + "{" + \
                TexFile.escapeString(heading) + "}\n"
    def addComment(self, comment, fileName, startLine, endLine):
        comment = TexFile.escapeString(comment)
        comment = re.sub("`([^`']*)'", "{\\\\tt \\1}", comment)
        self += re.sub("\"([^\"]*)\"", "``\\1''", comment)
    def addCode(self, code, fileName, startLine, endLine):
        code = code.rstrip()
        code = re.sub("\\\\end{Verbatim}", "\\\\_end{Verbatim}", code)
        code = re.sub("\t", "   ", code)
        self += "\n\\begin{Verbatim}[fontsize=\\small,frame=leftline,framerule=0.9mm," + \
                "rulecolor=\\color{gray},framesep=5.1mm,xleftmargin=5mm,fontfamily=cmtt]\n"
        self += code
        self +=  "\n\\end{Verbatim}\n"

class IdqTexFile(TexFile):
    def __init__(self, title, author = "id Quantique", style = "article", includeToc = True):
        TexFile.__init__(self, title, author, style, includeToc)

class BknrTexFile(TexFile):
    def __init__(self, title, author, style, includeToc):
        TexFile.__init__(self, title, author, style, includeToc)
        self.firstSection = True
    def beginning(self):
        return '\\chapter{' + TexFile.escapeString(self.title) + '}\n'
    def ending(self):
        return ''
    def addComment(self, comment, fileName, startLine, endLine):
        comment = TexFile.escapeString(comment)
        self += re.sub("\"([^\"]*)\"", "``\\1''", comment)
    def sectioningCommand(self, level):
        string = ""
        if level == 0:
            if self.firstSection == False:
                string = ""
#                string = "vbox{\n\\vspace{1cm}\n\\centering\n" + \
#                         "\\includegraphics[scale=0.6]{peecol" + \
#                         str(random.randint(1, 10) ) +  "}}\n\n\\"
            else:
                self.firstSection = False
        if self.style == "article":
            return string + self.articleSectioningCommands[min(level, len(self.articleSectioningCommands))]
        elif self.style == "book":
            return string + self.bookSectioningCommands[min(level, len(self.bookSectioningCommands))]

class Pbook:
    def __init__(self, files, outFile):
        self.files = files
        self.commentRe = None
        self.headingRe = None
        self.removeRe = None
        self.outFile = outFile
        self.lineCounter = 0
        if not self.outFile.title: self.outFile.title = basename(file)

    def formatBuffer(self):
        self.outFile.reset()
        self.lineCounter = 0
        for file in self.files:
            data = open(file, "r").read()
            if self.removeRe:
                data = self.removeRe.sub("", data)
            # search the first heading
            startMatch = self.headingRe.search(data)
            if not startMatch:
                raise "File must have at least one heading"
            self.lineCounter += len(data[:startMatch.start()].split('\n'))
            data = data[startMatch.start():]
            self.fileName = file

            lines = data.split('\n')
            while len(lines) > 0:
                line = lines[0]
                if re.match("^\s*$", line):
                    lines.pop(0)
                    self.lineCounter += 1
                    continue
                elif self.figureRe.match(line):
                    line = lines.pop(0)
                    self.doFigure(line)
                    self.lineCounter += 1
                elif self.escapeRe.match(line):
                    line = lines.pop(0)
                    self.doEscape(line)
                    self.lineCounter += 1
                elif self.headingRe.match(line):
                    line = lines.pop(0)
                    self.doHeading(line)
                    self.lineCounter += 1
                elif self.commentRe.match(line):
                    self.doComment(lines)
                else:
                    self.doCode(lines)

    def doHeading(self, line):
        match = self.headingRe.match(line)
        assert(match != None)
        level = len(match.group(1)) - 1
        headingName = line[match.end():]
        self.outFile.addHeading(level, headingName, self.fileName, self.lineCounter)

    def doFigure(self, line):
        match = self.figureRe.match(line)
        assert(match != None)
        figureFile = match.group(1)
        figureName = match.group(3)
        figureScale = match.group(2)
        self.outFile.addFigure(figureFile, figureScale, figureName, self.fileName, self.lineCounter)

    def doEscape(self, line):
        match = self.escapeRe.match(line)
        assert(match != None)
        escape = match.group(1)
        self.outFile.addEscape(escape, self.fileName, self.lineCounter)

    def doComment(self, lines):
        comment = ""
        lineCount = 0
        while len(lines) > 0:
            line = lines[0]
            match = self.commentRe.match(line)
            if not match: break
            line = lines.pop(0)
            lineCount += 1
            comment += line[:match.start()] + line[match.end():] + "\n"
        self.outFile.addComment(comment, self.fileName, self.lineCounter, self.lineCounter + lineCount)
        self.lineCounter += lineCount

    def doCode(self, lines):
        lineCount = 0
        code = ""
        while len(lines) > 0:
            line = lines[0]
            if (self.headingRe.match(line) or self.escapeRe.match(line) \
                                           or self.figureRe.match(line) \
                                           or self.commentRe.match(line)):
                break
            line = lines.pop(0)
            lineCount += 1
            code += line + "\n"
        self.outFile.addCode(code, self.fileName, self.lineCounter, self.lineCounter + lineCount)
        self.lineCounter += lineCount

    def makeFile(self, fileName):
        self.outFile.reset()
        self.formatBuffer()
        return self.outFile.writeFile(fileName)

class LispPbook(Pbook):
    def __init__(self, files, outFile):
        Pbook.__init__(self, files, outFile)
        self.commentRe = re.compile('^;;;($|[^#f])', re.M)
        self.headingRe = re.compile('^;;;(#+)', re.M)
        self.figureRe = re.compile('^;;;f\s+\"(.+)\"\s+([^\s]+)\s+(.*)', re.M)
        self.escapeRe = re.compile('^;;;t\s+(.+)', re.M)

class CPbook(Pbook):
    def __init__(self, files, outFile):
        Pbook.__init__(self, files, outFile)
        self.commentRe = re.compile('^(\s|/)*\*\*($|[^f#/])', re.M);
        self.headingRe = re.compile("^/\*\*(#+)", re.M)
        self.removeRe = re.compile('\*\*+/', re.M)
        self.figureRe = re.compile('^/\*\*f \"(.+)\"\s+([^\s]+)\s(.*)', re.M)


def usage():
    print "Usage: ", sys.argv[0], " [-h] [-c TexFile|BknrTexFile|IdqTexFile|TxtFile] ", \
          "[-T C|Lisp] [-t title] [-a author] [-O] [-o output] [-s style] file ..."

def extToType(ext):
    fileExtToType = ( ((".c", ".cpp", ".C", ".h"), CPbook),
                      ((".lisp", ".el", ".l", ".cl"), LispPbook) )
    for types, typeClass in fileExtToType:
        if (ext in types):
            return typeClass
    return None

def main():
    texClass = TexFile
    type = None
    output = None
    (author, title, toc, style) = (None, None, True, "article")
    try:
        opts, args = getopt.getopt(sys.argv[1:], "hc:T:t:a:Oo:s:")
        if not args:
            raise getopt.error, "At least one file argument required"
        for optname, optvalue in opts:
            if optname == "-h":
                usage()
                return
            elif optname == "-c":
                if optvalue == "TexFile":
                    texClass = TexFile
                elif optvalue == "IdqTexFile":
                    texClass = IdqTexFile
                elif optvalue == "BknrTexFile":
                    texClass = BknrTexFile
                elif optvalue == "TxtFile":
                    texClass = TxtFile
                else:
                    raise getopt.error, "Unknown TexFile class ", optvalue
            elif optname == "-t":
                title = optvalue
            elif optname == "-a":
                author = optvalue
            elif optname == "-O":
                toc = False
            elif optname == "-s":
                style = optvalue
            elif optname == "-T":
                if optvalue == "C":
                    type = CPbook
                elif optvalue == "Lisp":
                    type = LispPbook
                else:
                    raise getopt.error, "Unknown pbook file type ", optvalue
            elif optname == "-o":
                output = optvalue
    except getopt.error, msg:
        print msg
        usage()
        return 1
    file = args[0]
    name,ext = splitext(file)
    if not title:
        title = basename(name)
    if not type:
        type = extToType(ext)
        if not type:
            print "Could not get type for ", ext
            return 2
    pbook = type(args, texClass(title, author, style, toc))
    if not output:
        output = basename(name)
    try:
        file = pbook.makeFile(output)
        if output != "-":
            print "Wrote output to ", file.name
    except IOError, (errno, strerror):
        print "Caught an error while generating \"%s\": %s" % (output, strerror)
    except:
        print "Caught an error while generating \"%s\"" % (output)
        return 1

if __name__ == "__main__":
    sys.exit(main())
