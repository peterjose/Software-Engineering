############
# Dummy code created for SE assignment 4
############

#TODO: Make the function recursive

import csv

fileName = 'X.csv'

# @param list_A, feature list of all configuration
# @param list_B, performance list of all configuration
def cal_fn(list_A, list_B):
    countL, countR= list_A.count(1),list_A.count(0)
    sumL, sumR = 0,0
    for i in range(len(list_A)) :
        if list_A[i] == 1:
            sumL += list_B[i]
        else:
            sumR += list_B[i]
    meanL,meanR = sumL, sumR
    if sumL != 0:
        meanL /= countL
    if sumR != 0:
        meanR /= countR
    meanL, meanR = round(meanL,2), round(meanR,2)
    sq_errL, sq_errR = 0,0
    for i in range(len(list_A)) :
        if list_A[i] == 1:
            sq_errL += pow((meanL-list_B[i]),2)
        else:
            sq_errR += pow((meanR-list_B[i]),2)

    sq_errL,sq_errR = round(sq_errL , 2), round(sq_errR , 2)
    sq_errT = round(sq_errL + sq_errR,2)
    meanT = 0 
    meanT = round((meanL+meanR),2) if sumL == 0 or sumR == 0 else round((meanL+meanR)/2,2)
    return [countL,meanL,sq_errL,countR,meanR,sq_errR,meanT,sq_errT]

# Main
if __name__ == '__main__':
    #open and read the csv file
    csvfile = open(fileName)
    reader = csv.DictReader(csvfile)
    columnName = list(dict(list(reader)[0]).keys())
    csvfile.seek(0)
    input_matrix = []
    length = len(input_matrix)
    for row in reader:
        for i in range(1, len(columnName)):
            if len(input_matrix) != len(columnName)-1:
                input_matrix.append([])
            if length != 0:
                input_matrix[i-1].append(int(row[columnName[i]]))
        length = len(input_matrix)
    csvfile.close()

    # process the data
    output_matrix = []
    min_error = -1
    feature_mean,feature_index = 0,0
    feature = ''
    outputFile = open("output.txt",'a')
    outputFile.write("\n\n" + fileName)
    if len(input_matrix[0]) <= 1:
        print("\nReached End Node")
        outputFile.write("\n********No splitting Required, reached end node*****\n")
        exit()

    outputFile.write("\ncountL, meanL, sq_errL, countR, meanR, sq_errR, mean_T, sq_errT")
    for i in range(0,len(input_matrix)-1):
        print(columnName[i+1])
        output_matrix.append(cal_fn(input_matrix[i],input_matrix[-1]))
        print("\t",output_matrix[-1])
        outputFile.write("\n")
        outputFile.write(columnName[i+1])
        outputFile.write(str(output_matrix[-1]))
        # find the minimum sum of square error losses from all the features 
        if min_error == -1:
            min_error = output_matrix[-1][-1]
            feature_mean = output_matrix[-1][-2]
            feature = columnName[i+1]
        if min_error >= output_matrix[-1][-1]:
            if len(input_matrix[0]) > 2 or (
                (len(input_matrix[0]) == 2) and 
                output_matrix[-1][0] == output_matrix[-1][3]):
                feature = columnName[i+1]
                min_error = output_matrix[-1][-1]
                feature_mean = output_matrix[-1][-2]
                feature_index = i
    # generate the output
    print("Split based on", feature, min_error, output_matrix[feature_index])
    outputFile.write("\nSplit based on ")
    outputFile.write(feature+" " +str(min_error)+ " " +str(output_matrix[feature_index]) + "\n")
    fName = fileName.split(".")
    if fName[0][-1] == 'R':
        outputFile.write("\nsuccessor_right:")
    if fName[0][-1] == 'L':
        outputFile.write("\nsuccessor_left:")
    outputFile.write("\ndatapoints: "+ str(len(input_matrix[0]))+
        "\nerror_of_split: "  + str(min_error)+
        "\nmean: "+ str(feature_mean)+
        "\nname: "+fName[0]+
        "\nsplit_by_feature: "+feature)
    outputFile.close()

    # create output files
    outputL = open(fName[0]+"L.csv",'w',newline ='')
    outputR = open(fName[0]+"R.csv",'w',newline ='')

    # remove the feature that was used before
    newColumnName = columnName.copy()
    newColumnName.remove(feature)

    # open files for the output
    csv.writer(outputL).writerow(newColumnName)
    csv.writer(outputR).writerow(newColumnName)

    csvfile = open(fileName)
    reader = csv.DictReader(csvfile)
    for row in reader:     
        tmp_row = []
        for col_inx in newColumnName:
            tmp_row.append(row[col_inx])
        csv.writer(outputL).writerow(tmp_row) if row[feature] == '1' else csv.writer(outputR).writerow(tmp_row)

    #Close the output files
    outputL.close()
    outputR.close()