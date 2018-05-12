// https://grandgames.net/nonograms/picross/...


def (String name, String topMetadata, String leftMetadata) = init()

def sb = new StringBuilder()
def prefix = translit(name)
sb.append("${prefix}.name=$name\n")
sb.append("${prefix}.horizontal=\\\n${reformatTopMetadata(topMetadata)}\n\n")
sb.append("${prefix}.vertical=${reformatLeftMetadata(leftMetadata)}\n")
println sb

def reformatTopMetadata(String string) {
    def matrix = string.split('\n')
            .findAll { !it.isEmpty() }
            .collect { line ->
        line.replace("    ", "\t")
                .split(/\t/)
    }

    def (int width, int height) = [matrix.last().size(), matrix.size()]
    println "top metadata: width=$width, height=$height"

    def list = []

    for (int x in 0..<width) {
        def builder = new StringBuilder()
        for (y in 0..<height) {
            List<String> line = matrix[y]
            if (line.size() > x && !line.get(x)?.isEmpty())
                builder.append(" ${line.get(x)}")
        }
        list += builder.toString().trim()
    }
    reformatMetadata(list)
}


def reformatLeftMetadata(String string) {
    def matrix = string.split('\n')
            .findAll { !it.isEmpty() }
            .collect { line -> line.replace("    ", "\t").split(/\t/)
    }

    def (int width, int height) = [matrix*.size().max(), matrix.size()]
    println "left metadata: width=$width, height=$height"

    reformatMetadata(matrix.collect { it.join(" ").trim() })
}

def reformatMetadata(List<String> strings) {
    def builder = new StringBuilder()
    for (i in 0..<strings.size()) {
        builder.append("${strings[i]}, ")
        if ((i - 4) % 5 == 0 && i != strings.size() - 1) {
            builder.append("\\\n ")
        }
    }

    builder.setLength(builder.size() - ", ".size())
    builder
}



def translit(String name) {
    def cyr = "абвгдеёжзийклмнопрстуфхцчшщъюьэюя 1234567890"
    def lat = "abvgdeegziiklmnoprctufhc4ss_u_euy_1234567890"

    name.toLowerCase()
            .collect {cyr.indexOf(it)}
            .findAll{it != -1}
            .collect { return lat[it] }
            .join("")
}

def init() {
    // copy-paste from top of puzzle:
    def name = "Девушка с кошкой"
    def topMetadata = """
                                                                                                                                                                                                                                                                                                            4
                                                                                                                                                                                                                                                                                                            3			1
                                                                                                                                                                                                                                                                        1									3			13
                                                                                                                                                                                                                                                                    2	2	1				1		4	3	2		6	1
                                                                                                                                                                                                                                                            2		2	2	2				2	1	2	2	1	7	2	1	2	3
                15								2																							1																2	3	1										3	2	1	1	1				1	2	1	2	1	2	4	3	8	1	4
                5	15	12			8	7	3	2	1																						1										4		2			2	5	7	1		1							1	1	2	1	2	1	2	2		1	1	2	2	1	4	1	21	3	10	2							2
        19		9	2	3			6	6	2	5	2															3							5		2						6		2	5	3	2	2	4	1	1	8	1	2						1	3	1	1	2	2	2	2	2		5	2	2	2	2	1	1	2	3	3	8	4			2			7
        11	21	2	7	1	13	14	5	4	12	12	2	1													4	8	1				1	1	1	3	3	2			2		2	3	2	2	2	3	4	1	2	2	1	2	4	1					3	2	2	2	3	2	2	2	1	1	2	3	2	2	19	1	24	2	21	22	27	2	4	3	4	1	5	3
    19	1	10	3	2	1	6	5	8	8	2	2	19	5	1						7	6	5	4		5	1	1	2			2	4	2	2	5	1	3	2	2	2	3	2	2	1	2	2	1	1	2	2	2	2	9	3	2	2	2	2	1	1	4	3	2	4	2	2	2	2	1	3	4	3	21	2	23	2	2	3	3	4	39	3	3	33	5	6	6	1
    17	2	2	3	4	6	2	8	2	2	12	13	2	1	6	1		4			5	8	3	2		1	3	3	3			6	6	6	2	2	5	2	3	2	2	2	1	2	1	1	3	2	2	3	1	1	2	2	5	4	3	4	4	2	6	7	4	3	3	2	7	5	4	2	15	20	17	2	2	2	3	1	2	2	3	3	42	38	2	5	11	7	8
    1	7	2	11	14	2	4	3	10	11	2	1	12	17	2	6	8	3	4	9	6	3	11	7	6	10	4	2	2	2	2	6	1	2	3	4	2	1	2	7	2	2	2	2	2	1	1	1	5	1	2	2	2	1	4	14	4	18	2	7	5	2	3	9	4	1	12	5	4	2	4	4	2	2	1	3	1	3	1	1	2	1	2	1	1	9	9	3	4
    5	5	15	1	8	31	2	32	20	19	20	21	2	2	16	2	17	17	17	10	4	11	13	11	7	4	2	6	4	12	19	4	3	7	1	3	2	3	2	1	4	2	1	2	1	2	2	3	2	2	2	1	2	2	2	6	21	4	9	4	2	2	2	4	4	10	2	13	25	2	3	4	7	6	2	2	2	2	2	1	1	2	1	2	4	14	3	2	23
37	13	5	1	1	2	1	31	2	1	2	2	3	26	13	2	15	3	3	4	4	12	1	1	3	12	22	20	19	29	2	2	2	1	2	5	1	2	2	3	1	3	2	2	2	1	2	2	2	1	1	1	1	1	1	2	1	2	2	4	3	2	8	8	12	10	11	4	6	2	1	6	4	2	2	3	2	3	1	2	3	3	3	3	2	2	3	1	3	3
5	1	2	1	3	2	2	1	1	2	1	2	1	2	27	44	3	17	17	16	16	18	16	14	13	13	9	9	4	4	26	10	6	8	4	6	17	4	4	2	5	4	3	1	1	2	3	2	3	2	3	2	4	8	1	14	2	2	2	4	7	10	4	3	3	3	2	2	5	2	38	2	1	2	2	1	5	2	3	3	3	2	2	3	9	5	2	2	9	9
40	5	2	1	1	4	6	11	7	6	5	4	3	2	4	2	46	27	28	28	31	16	16	16	16	15	3	2	5	3	4	6	7	8	9	3	2	4	3	5	1	1	1	1	3	1	1	2	3	4	5	7	11	14	29	4	4	3	3	3	3	3	2	2	2	2	2	2	1	1	1	1	1	1	1	1	1	1	1	6	2	2	1	2	3	6	29	25	2	4
"""

    def leftMetadata = """
30	11	12	5	8	9
13	12	2	8	7	3	7	4	7	3
12	15	2	3	2	2	3	3	1	7	5	3
11	14	2	2	3	8	2	3	3	4	3	2
11	5	4	2	2	2	15	2	3	3	3	1	3	1
11	6	3	2	1	2	4	12	2	3	3	5	7	1
12	5	2	2	1	3	3	1	5	1	6	5	6	1
10	2	4	1	1	1	2	3	4	2	2	6	3	2
9	2	2	2	1	2	2	4	1	2	4	3
10	3	2	2	1	3	3	4	2	3	7	5
11	1	5	2	1	2	2	5	3	14
11	2	4	2	4	2	4	4	2	2	12
6	4	3	3	2	3	2	5	2	5	1	11
6	3	8	3	3	2	1	2	1	2	2	9
7	2	9	2	2	2	1	5	3	3	6	1
4	2	1	8	3	4	2	5	3	3	10
5	4	9	2	5	1	1	5	3	4	1	8
6	4	9	2	6	2	1	2	5	3	1	11
6	15	2	2	3	8	1	2	3	3	1	2	7
1	2	15	2	5	5	1	2	12	1	1	9
2	2	16	2	3	3	5	1	2	8	2	1	1	8
2	11	2	1	1	2	2	13	3	1	1	9
3	15	1	5	2	30	1	1	2	8
5	14	2	12	6	16	4	12
6	12	2	18	6	11	2	3	3	6	3
7	11	3	2	3	5	4	14	6	3	7	3
18	3	2	4	1	2	4	1	4	2	5	14	2
17	4	2	3	1	1	1	2	1	3	4	4	11	2
16	3	2	1	2	1	6	3	25	1
14	4	1	2	1	2	4	10	14	1
7	3	1	1	2	2	4	2	4	16	1
5	10	2	1	1	1	1	1	2	24	1
4	10	3	3	2	2	1	5	18	1
2	5	9	3	3	2	2	1	18	1
2	2	3	5	1	11	2	2	1	18	1
4	4	3	1	1	7	2	1	1	19	1
2	5	3	2	3	1	2	2	19	1
11	3	4	3	2	1	20	1
1	15	4	1	2	1	20	2
7	16	4	2	1	23	2
1	25	3	2	2	1	1	22	2
2	24	3	6	2	23	2
4	27	8	2	1	2	20	3
4	26	2	5	2	2	19	4
4	27	8	4	15	8
4	27	6	2	15	6	1
2	19	7	1	4	6	7	5	2
20	2	7	1	2	2	8	6	1
21	1	7	1	3	20	1	3
18	2	7	1	3	3	13	5
1	7	1	7	7	5	3	6	4	3
1	7	1	1	16	7	4	8	2	1	3
1	8	3	34	13	9
1	8	5	19	21	12
1	15	17	2	2	12	6	2
1	16	18	1	2	3	4	3	3
2	32	4	2	2	3	3	5
2	32	2	2	3	3	3	2	3
2	26	5	2	2	4	3	2	4
2	25	2	2	3	3	3	1	3
3	23	1	3	3	2	2	3
3	21	2	3	6	2	4
3	21	2	3	5	2	4
3	20	2	4	4	3	2	2
3	19	2	4	4	4	1	1
2	18	1	3	4	3	2	1
2	18	2	3	4	4
2	17	2	2	5	3
2	17	2	3	4	2
1	18	2	2	3	3
1	1	14	2	2	3	3
1	1	14	2	2	3	4
1	1	13	2	1	2	2	1
1	2	13	2	1	4	1
1	2	13	2	1	4	1
1	1	2	13	2	4	1
1	1	18	3	6
1	2	17	3	6
1	2	16	3	2	4
1	2	16	4	2	4
1	3	15	3	2	3
1	1	3	14	3	2	3
1	1	3	1	13	3	2	3
2	2	3	9	2	1	4	1	3
1	4	2	9	1	2	4	2	4
2	5	1	10	1	5	2	3
2	7	1	5	3	2	8	2	3	1
2	9	5	2	2	14	2	4	1
3	9	6	1	21	3	7
13	4	47
"""

    return [name, topMetadata, leftMetadata]
}