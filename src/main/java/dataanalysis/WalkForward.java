package dataanalysis;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.javatuples.Pair;

import com.opencsv.exceptions.CsvValidationException;

import datamining.CsvCreator;
import datamining.Utility;

import weka.core.Instances;

import weka.classifiers.Evaluation;
import weka.classifiers.bayes.NaiveBayes;
import weka.core.converters.ConverterUtils.DataSource;

public class WalkForward {
    
    private static final String CSV_JIRA = "02-ticketdata.csv";
    private static final String CSV_VERSIONS = "03-versionsdata.csv";
    private static final String CSV_METHRICS = "04-data.csv";
    private static final String CSV_TRAINING = "training.csv";
    private static final String CSV_TESTING = "testing.csv";
    private static final String ARFF_TRAINING = "training.arff";
    private static final String ARFF_TESTING = "testing.arff";

    public static  List<Pair<List<String>, String>> versionSplit() throws CsvValidationException, IOException{
        List<List<String>> versions = Utility.csvToList(CSV_VERSIONS);
        Integer i;
        Integer j;
        List<Pair<List<String>, String>> iteration = new ArrayList<>();
        for(i=2; i<versions.size(); i++){
            String testing = versions.get(i).get(0);
            List<String> training = new ArrayList<>();
            for(j=1; j<i; j++){
                training.add(versions.get(j).get(0));
            } 
            iteration.add(new Pair<>(training, testing));
        }

        return iteration;
    }

    public static List<List<Object>> filesFromVersion(String version) throws CsvValidationException, IOException{
        Integer i=0;
        Integer j;
        List<List<Object>> csvLines = new ArrayList<>();
        List<List<String>> files = Utility.csvToList(CSV_METHRICS);

        while(i<files.size() && !files.get(i).get(0).equals(version)){
            i++;
        }
        while(i<files.size() && files.get(i).get(0).equals(version)){
            List<Object> line = new ArrayList<>();
            for(j=2; j<files.get(i).size()-1; j++){
                line.add(Float.valueOf(files.get(i).get(j)));
            }
            line.add(files.get(i).get(files.get(i).size()-1));

            i++;
            csvLines.add(line);
        }
        return csvLines;
    }

    public static void wekaApi() throws Exception{
        Utility.csvToArff(CSV_TRAINING, ARFF_TRAINING);
        Utility.csvToArff(CSV_TESTING, ARFF_TESTING);
        
        /*DataSource source1 = new DataSource(ARFF_TRAINING);
        Instances training = source1.getDataSet();

        DataSource source2 = new DataSource(ARFF_TESTING);
        Instances testing = source2.getDataSet();

        int numAttr = training.numAttributes();
        training.setClassIndex(numAttr - 1);
        testing.setClassIndex(numAttr - 1);

        NaiveBayes classifier = new NaiveBayes();

        classifier.buildClassifier(training);

        Evaluation eval = new Evaluation(testing);	

        eval.evaluateModel(classifier, testing); 
        
        System.out.println("AUC = "+eval.areaUnderROC(1));
        System.out.println("kappa = "+eval.kappa());
        System.out.println("precision = "+eval.precision(1));
        System.out.println("recall = "+eval.recall(1));*/
    
        
	}


    public static void walkFwIncremental() throws Exception{
        CsvCreator.data(CSV_JIRA, true);
        Integer i;
        Integer j;
        Integer k;
        File testingFile = new File(CSV_TESTING);
        File trainingFile = new File(CSV_TRAINING);
        List<Pair<List<String>, String>> trainingTesting = versionSplit();

        for(i=0; i<trainingTesting.size();i++){
            List<String> training = trainingTesting.get(i).getValue0();
            String testing = trainingTesting.get(i).getValue1();
            try(FileWriter trainingWriter = new FileWriter(trainingFile);
                FileWriter testingWriter = new FileWriter(testingFile);){
                trainingWriter.append("LOCTouched,LOCadded,MaxLOCadded,AvgLOCadded,churn,MaxChurn,AvgChurn,ChgSetSize,MaxChgSet,AvgChgSet,Bugginess\n");
                for(j=0;  j<training.size(); j++){
                    List<List<Object>> csvTraining = filesFromVersion(training.get(j));
                    for(k=0;k<csvTraining.size();k++){
                        //String line = csvTraining.get(k).replace("[","");
                        //line = line.replace("]","");
                        //line = line.replace(" ","");
                        trainingWriter.append(csvTraining.get(k) +"\n");
                        trainingWriter.flush();
                    }
                }
                testingWriter.append("LOCTouched,LOCadded,MaxLOCadded,AvgLOCadded,churn,MaxChurn,AvgChurn,ChgSetSize,MaxChgSet,AvgChgSet,Bugginess\n");
                List<List<Object>> csvTesting = filesFromVersion(testing);
                for(k=0; k<csvTesting.size(); k++){
                    //String line = csvTesting.get(k).replace("[","");
                    //line = line.replace("]","");
                    //line = line.replace(" ","");
                    testingWriter.append(csvTesting.get(k)+"\n");
                    testingWriter.flush();
                }
            }
            wekaApi();
        }
    }

    public static void main(String[] args) throws Exception{
        walkFwIncremental();
    }

}