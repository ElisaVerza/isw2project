package dataanalysis;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.javatuples.Pair;
import org.netlib.util.booleanW;

import com.opencsv.exceptions.CsvValidationException;

import datamining.CsvCreator;
import datamining.Utility;

import weka.core.Instances;

import weka.classifiers.Evaluation;
import weka.classifiers.bayes.NaiveBayes;
import weka.classifiers.lazy.IBk;
import weka.classifiers.trees.RandomForest;
import weka.core.converters.ConverterUtils.DataSource;

public class WalkForward {

    private static final Logger LOGGER = Logger.getLogger(WalkForward.class.getName());
    
    private static final String CSV_JIRA = "02-ticketdata.csv";
    private static final String CSV_METHRICS = "04-data.csv";
    private static final String CSV_TRAINING = "training.csv";
    private static final String CSV_TESTING = "testing.csv";
    private static final String CSV_FINAL = "finalresult.csv";
    private static final String ARFF_TRAINING = "training.arff";
    private static final String ARFF_TESTING = "testing.arff";
    private static final String PRJ_NAME = "Syncope";

    public static  List<Pair<List<String>, String>> versionSplit() throws CsvValidationException, IOException{
        Integer j;
        Integer i;
        List<String> versions = new ArrayList<>();
        List<List<String>> files = Utility.csvToList(CSV_METHRICS);
        List<Pair<List<String>, String>> iteration = new ArrayList<>();

        for(j=0; j<files.size(); j++){
            if(!versions.contains(files.get(j).get(0))){
                versions.add(files.get(j).get(0));
            }
        }

        for(i=2; i<versions.size(); i++){
            String testing = versions.get(i);
            List<String> training = new ArrayList<>();
            for(j=1; j<i; j++){
                training.add(versions.get(j));
            } 
            iteration.add(new Pair<>(training, testing));
        }
        return iteration;
    }

    public static List<String> filesFromVersion(String version) throws CsvValidationException, IOException{
        Integer i=0;
        Integer j;
        List<String> csvLines = new ArrayList<>();
        List<List<String>> files = Utility.csvToList(CSV_METHRICS);

        while(i<files.size() && !files.get(i).get(0).equals(version)){
            i++;
        }
        while(i<files.size() && files.get(i).get(0).equals(version)){
            StringBuilder line = new StringBuilder();
            for(j=2; j<files.get(i).size(); j++){
                line.append(files.get(i).get(j));
                if(j!=files.get(i).size()-1){
                    line.append(",");
                }
            }
            csvLines.add(line.toString());
            i++;
        }
        return csvLines;
    }

    public static Evaluation classifier(Instances training, Instances testing, int classifierIndex) throws Exception{
        Evaluation eval;
        switch (classifierIndex) {
            case 1:
                NaiveBayes classifierNB = new NaiveBayes();
                classifierNB.buildClassifier(training);
                eval = new Evaluation(testing);	
                eval.evaluateModel(classifierNB, testing); 
                break;
            case 2:
                RandomForest classifierRF = new RandomForest();
                classifierRF.buildClassifier(training);
                eval = new Evaluation(testing);	
                eval.evaluateModel(classifierRF, testing); 
                break;
            case 3:
                RandomForest classifierIBK = new RandomForest();
                classifierIBK.buildClassifier(training);
                eval = new Evaluation(testing);	
                eval.evaluateModel(classifierIBK, testing); 
                break;
            default:
                eval = new Evaluation(null);
                break;
        }
        
        return eval;
    }

    public static void wekaApi(int iteration, int classifierIndex) throws Exception{
        File results = new File(CSV_FINAL);
        String classifier = "";
        switch (classifierIndex) {
            case 1:
                classifier = "NaiveBayes";
                break;
            case 2:
                classifier = "RandomForest";
                break;
            case 3:
                classifier = "iBk";
                break;

        
            default:
                break;
        }
        
        //Costruzione classificatore
        Utility.csvToArff(CSV_TRAINING, ARFF_TRAINING);
        Utility.csvToArff(CSV_TESTING, ARFF_TESTING);
        
        DataSource source1 = new DataSource(ARFF_TRAINING);
        Instances training = source1.getDataSet();

        DataSource source2 = new DataSource(ARFF_TESTING);
        Instances testing = source2.getDataSet();

        int numAttr = training.numAttributes();
        training.setClassIndex(numAttr - 1);
        testing.setClassIndex(numAttr - 1);

        //Calcolo % of training
        List<List<String>> trainingFile = Utility.csvToList(CSV_TRAINING);
        List<List<String>> dataFile = Utility.csvToList(CSV_METHRICS);
        float trainingPerc = (trainingFile.size()/(float)dataFile.size())*100f;
        System.out.println(trainingPerc);

        Evaluation eval = classifier(training, testing, 1);
        try(FileWriter finalResults = new FileWriter(results, true)){
            if(iteration==1){finalResults.append("project,release,classifier,precision,recall,AUC,kappa\n");}

            finalResults.append(PRJ_NAME+","+iteration+","+classifier+","+eval.areaUnderROC(1)+","+eval.kappa()+","+
                                eval.precision(1)+","+eval.recall(1));
        }
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
                    List<String> csvTraining = filesFromVersion(training.get(j));
                    for(k=0;k<csvTraining.size();k++){
                        String line = csvTraining.get(k);
                        line = csvTraining.get(k).replace("[","");
                        line = line.replace("]","");
                        line = line.replace(" ","");
                        trainingWriter.append(line+"\n");
                        trainingWriter.flush();
                    }
                }
                testingWriter.append("LOCTouched,LOCadded,MaxLOCadded,AvgLOCadded,churn,MaxChurn,AvgChurn,ChgSetSize,MaxChgSet,AvgChgSet,Bugginess\n");
                List<String> csvTesting = filesFromVersion(testing);
                for(k=0; k<csvTesting.size(); k++){
                    String line = csvTesting.get(k).replace("[","");
                    line = line.replace("]","");
                    line = line.replace(" ","");
                    testingWriter.append(line+"\n");
                    testingWriter.flush();
                }
            }   
            wekaApi(i, 1);
        }
    }

    public static void main(String[] args) throws Exception{
        walkFwIncremental();
    }

}