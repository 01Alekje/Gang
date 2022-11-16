import javafx.animation.AnimationTimer;
import javafx.application.Application;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;
import javafx.stage.Stage;


import java.util.Arrays;

import static java.lang.Math.round;
import static java.lang.Math.sqrt;
import static java.lang.System.exit;
import static java.lang.System.out;

/*
 *  Program to simulate segregation.
 *  See : http://nifty.stanford.edu/2014/mccown-schelling-model-segregation/
 *
 * NOTE:
 * - JavaFX first calls method init() and then method start() far below.
 * - To test methods uncomment call to test() first in init() method!
 *
 */
// Extends Application because of JavaFX (just accept for now)
public class Neighbours extends Application {

    class Actor {
        final Color color;        // Color an existing JavaFX class
        boolean isSatisfied;      // false by default

        Actor(Color color) {      // Constructor to initialize
            this.color = color;
        }  // Constructor, used to initialize
    }

    // Below is the *only* accepted instance variable (i.e. variables outside any method)
    // This variable may *only* be used directly in methods init() and updateWorld()
    Actor[][] world;              // The world is a square matrix of Actors

    // This is the method called by the timer to update the world
    // (i.e move unsatisfied) approx each 1/60 sec.
    void updateWorld() {
        // % of surrounding neighbours that are like me
        double threshold = 0.7;
        makeSatisfied();


        // TODO
    }

    // returns array where [0] is number of same color, and [1] is different neighbours
    int[] countNeighbours(int r, int c) {
        int[] neighbours = {0, 0};

        if (world[r][c] != null) {
            Actor actor = world[r][c];

        for (int i = -1; i < 2; i++) {
            for (int j = -1; j < 2; j++) {
                if (isValidLocation(world.length, (r + i), (c + j))) {


                    if (world[r + i][c + j] != null) {
                        if (world[r + i][c + j].color == actor.color) {
                            neighbours[0]++;
                        } else {
                            neighbours[1]++;
                        }
                    }
                }
            }
        }
    }
        return neighbours;
    }

    boolean isSatisfied(int[] neighbours, double threshold) {
        if (neighbours[0] > threshold * (neighbours[0] + neighbours[1])){
            return true;
        }
        return false;
    }

    Actor[] getUnsatisfied(){
        Actor[] tmp = new Actor[countNotsatisAndNulls()[0]];
        int k = 0;

        for (int r = 0; r < world.length; r++){
            for (int c = 0; c < world[0].length; c++){
                if (!isSatisfied(countNeighbours(r, c),0.7)){
                    tmp[k] = world[r][c];
                    world[r][c] = null;
                    k++;
                }
            }
        }
        out.println(Arrays.toString(tmp));
        return tmp;
    }

    int[] getNullPos (){
        int[] tmp = new int[countNotsatisAndNulls()[1]];
        int j = 0;

        if (tmp.length > 0) {
            for(int r = 0; r < world.length; r++) {
                for (int c = 0; c < world[0].length; c++) {
                    if (world[r][c] == null) {
                        tmp[j] = r * world.length + c;
                        j++;
                    }
                }
            }
        }

        return tmp;
    }

    int[] countNotsatisAndNulls(){
        int[] counter = {0,0};

        for (int r = 0; r < world.length; r++){
            for (int c = 0; c < world[0].length; c++){
                if (!isSatisfied(countNeighbours(r, c),0.7)){
                    counter[0]++;
                } else if (world[r][c] == null){
                    counter[1]++;
                }
            }
        }
        return counter;
    }

    Actor[][] makeSatisfied(){
        Actor[][] newWorld = copy(world);
        Actor[] unsatisfied = getUnsatisfied();
        int[] nullist = getNullPos();
        shuffle(nullist);

        if (nullist.length > 0) {
            for (int i = 0; i < unsatisfied.length; i++){
                int tmpr = nullist[i] / world.length;
                int tmpc = nullist[i] % world.length;
                newWorld[tmpr][tmpc] = unsatisfied[i];
            }
        }

        return newWorld;
    }

    Actor[][] copy(Actor[][] m) {
        // Create the copy matrix
        Actor[][] cpy = new Actor[m.length][m.length];
        for (int r = 0; r < m.length; r++) {      // Copy values from m to cpy
            for (int c = 0; c < m[0].length; c++) {
                cpy[r][c] = m[r][c];
            }
        }
        return cpy;
    }

    int[] matrix2Array(int[][] m) {
        int[] arr = new int[m.length * m[0].length];
        int nCols = m[0].length;
        for (int r = 0; r < m.length; r++) {
            for (int c = 0; c < m[0].length; c++) {
                arr[r * nCols + c] = m[r][c];  // Math here
            }
        }
        return arr;
    }

    int[][] toMatrix(int[] arr){
        int rows = (int) sqrt((double) arr.length);
        int k = 0;

        int[][] m = new int[rows][rows];
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < rows; c++) {
                m[r][c] = arr[k];
                k++;
            }
        }
        return m;
    }

    void shuffle(int[] arr) {
        for (int i = arr.length; i > 1; i--) {
            int j = (int) round(arr.length * Math.random());
            int tmp = arr[j];   // Swap values
            arr[j] = arr[i - 1];
            arr[i - 1] = tmp;
        }
    }

    // This method initializes the world variable with a random distribution of Actors
    // Method automatically called by JavaFX runtime
    // That's why we must have "@Override" and "public" (just accept for now)
    @Override
    public void init() {
        //test();    // <---------------- Uncomment to TEST, see below!

        // %-distribution of RED, BLUE and NONE
        double[] dist = {0.25, 0.25, 0.50};
        // Number of locations (places) in world (must be a square)
        int nLocations = 900;   // Should also try 90 000

        // TODO
        this.world = seed(nLocations, dist);

        // Should be last
        fixScreenSize(nLocations);
    }

    // TODO Many methods here, break down of init() and updateWorld()

    Actor[][] seed(int locations, double[] dist){
        int length = (int) round(sqrt(locations));
        Actor[][] world = new Actor[length][length];
        double random;

        for(int r = 0; r < length; r++){
            for(int c = 0; c < length; c++){
                random = Math.random();
                if (random < 0.25){
                    world[r][c] = new Actor(Color.RED);
                } else if(random < 0.5) {
                    world[r][c] = new Actor(Color.BLUE);
                }
            }
        }
        return world;
    }

    // Check if inside world
    boolean isValidLocation(int size, int row, int col) {
        return 0 <= row && row < size && 0 <= col && col < size;
    }

    Actor doIt(Actor[][] world){
        return world[0][0];
    }

    // ----------- Utility methods -----------------

    // TODO Method to change format of data, generate random etc.

    // ------- Testing -------------------------------------

    // Here you run your tests i.e. call your logic methods
    // to see that they really work. Important!!!!
    void test() {
        // A small hard coded world for testing
        Actor[][] testWorld = new Actor[][]{
                {new Actor(Color.RED), new Actor(Color.RED), null},
                {null, new Actor(Color.BLUE), null},
                {new Actor(Color.RED), null, new Actor(Color.BLUE)}
        };
        double th = 0.5;   // Simple threshold used for testing

        //out.println(doIt(testWorld).color == Color.RED);

        int size = testWorld.length;
        out.println(isValidLocation(size, 0, 0));   // This is a single test
        out.println(!isValidLocation(size, -1, 0));
        out.println(!isValidLocation(size, 0, 3));

        // TODO  More tests here. Implement and test one method at the time
        // TODO Always keep all tests! Easy to rerun if something happens

        exit(0);
    }

    // ******************** NOTHING to do below this row, it's JavaFX stuff  **************

    double width = 500;   // Size for window
    double height = 500;
    final double margin = 50;
    double dotSize;

    void fixScreenSize(int nLocations) {
        // Adjust screen window
        dotSize = (double) 9000 / nLocations;
        if (dotSize < 1) {
            dotSize = 2;
        }
        width = sqrt(nLocations) * dotSize + 2 * margin;
        height = width;
    }

    long lastUpdateTime;
    final long INTERVAL = 450_000_000;


    @Override
    public void start(Stage primaryStage) throws Exception {

        // Build a scene graph
        Group root = new Group();
        Canvas canvas = new Canvas(width, height);
        root.getChildren().addAll(canvas);
        GraphicsContext gc = canvas.getGraphicsContext2D();

        // Create a timer
        AnimationTimer timer = new AnimationTimer() {
            // This method called by FX, parameter is the current time
            public void handle(long now) {
                long elapsedNanos = now - lastUpdateTime;
                if (elapsedNanos > INTERVAL) {
                    updateWorld();
                    renderWorld(gc);
                    lastUpdateTime = now;
                }
            }
        };

        Scene scene = new Scene(root);
        primaryStage.setScene(scene);
        primaryStage.setTitle("Simulation");
        primaryStage.show();

        timer.start();  // Start simulation
    }


    // Render the state of the world to the screen
    public void renderWorld(GraphicsContext g) {
        g.clearRect(0, 0, width, height);
        int size = world.length;
        for (int row = 0; row < size; row++) {
            for (int col = 0; col < size; col++) {
                int x = (int) (dotSize * col + margin);
                int y = (int) (dotSize * row + margin);
                if (world[row][col] != null) {
                    g.setFill(world[row][col].color);
                    g.fillOval(x, y, dotSize, dotSize);
                }
            }
        }
    }

    public static void main(String[] args) {
        launch(args);
    }

}
