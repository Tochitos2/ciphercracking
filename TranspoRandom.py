import math, itertools

reader = open("CommonWords.txt", "r")
commonWords = reader.readlines()

cypher = "IRRENREBOTHHBOHTREOFCADNOREFESDVNEFVORMTOLNWEOTNKLDVIYDNANUOOBCTLSTTLODSEHGEBNCLNSWHEALETGSEORPFSREBNIAIISTSEEHEDTRAIYNRHRNTWHAEERNETSIREBLETGEFSRHEAEORRAEBECTHRSBTTEMFRTFRLARMUHNSEEUSUBOHTOTRLEOAUTILEAENOPIYESDFRONTFIESASITRUIAAEINTTINEERKIWHDVLIWEIDSEHETLNEEHNIAEAESTUMRIEREFSYSYAOHUOWTMNHTSMTEILBSIWNDMFMEREERSHTYSGADPUAEBTAANNTRLBAEHIDNEMSNDANLIWEYGTIMRINAAETNSMLHNIAPYUUATOHDIHEECNTDDTPHNEIRVHEICOIETTETRFAGNNTATRELSNRACYENKMUMWKTODAAAWHOORLSTMATBLREILEDAIRSSLLOEHTHBLBNLTHVINHDDUSLAWHEAAOGCHNRTNORHRTCSPTTEEHNERMYANIOTIENALGOECATHYASDAGNYENEAHRTILCUYODFHLEFTOFYOEEGOAEONALLYHEUOEYOHETHEETALTRDNRFBHDEKMIOAEEAHLTEINCUEALEELHERNNDMOEGARDMIIOOATIWWXDSGWIPCNGTPHNICRFHFTOSHEXTCBTMGBNDYFENRDEOCERUIENHERTTRLRHEELREHYDTOHSSOTOOOSOUIEGAAFAEHWHDOALTDNYUIKLFBTDACFLECASLHENITGIHOEHELDPIEATYSHSUCUGATTACRGTNNBTOSLNASPHDLOMTDINMWORRRTOTSRTVSSEDF"

keyLength = 6
columnLength = math.ceil(len(cypher) / keyLength)
columns = [cypher[i:i+columnLength] for i in range(0, len(cypher), columnLength)]
plaintext = ""
permutations = list(itertools.permutations([0,1,2,3,4,5]))
results = []
topResult = ""
topCount  = 0

for perm in permutations:
    plaintext = ""
    for i in range(columnLength): 
        for k in perm:
            plaintext += columns[k][i]

    results.append(plaintext)

for result in results:
    count = 0
    for word in commonWords:
        count += result.count(word.upper().strip())
        # print(result.count(word.upper()))
    if(count > topCount): 
        topResult = result
        topCount  = count
        

print(topResult[:30])
