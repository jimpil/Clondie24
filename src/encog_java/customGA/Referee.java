package encog_java.customGA;

import java.util.Random;
import java.util.Map;

import org.encog.neural.networks.BasicNetwork;
import org.encog.neural.networks.training.CalculateScore;
import org.encog.ml.genetic.genome.Genome;
import org.encog.ml.MLRegression;
import org.encog.ml.genetic.population.Population;

import clojure.lang.IFn;
import clojure.lang.RT;
import clojure.lang.Symbol;
import clojure.lang.Keyword;


/*This is the scoring object the genetic algorithm needs to tell the
 *network how well it is doing.
 * 
 * */
public final class Referee implements CalculateScore 
{
        
        private static final IFn requireFn = RT.var("clojure.core", "require").fn();
        //private static final IFn keywordFn = RT.var("clojure.core", "keyword").fn();
         //{ requireFn.invoke(Symbol.intern("Clondie24.lib.core")); }
        private final IFn fitnessFn;  // RT.var("Clondie24.lib.core", "ga-fitness").fn(); //the fn we need
        private final IFn genPlayerFn; //RT.var("Clondie24.lib.core", "neural-player").fn();
        private final IFn tournaFn;
        private final Random generator;
       // private final Keyword oppoSearcher;
        private final int totalGames;
        private final Map<Keyword, Object> GAME;
	
	private Population population;
	
	public Referee(Map<Keyword, Object> game, IFn tournaFn, int tgames){ //prepare java-clojure interop
	   GAME = game;
	   requireFn.invoke(Symbol.intern("Clondie24.lib.core"));
	   fitnessFn =   RT.var("Clondie24.lib.core", "ga-fitness").fn();//the fn we need
	   genPlayerFn = RT.var("Clondie24.lib.core", "neural-player").fn();
	   generator = new Random();
	   this.tournaFn = tournaFn;
	   //this.oppoSearcher = oppoSearcher;
	   totalGames = tgames; 
	}
	
	public Referee(Map<Keyword, Object> game,  IFn tournaFn){
	   this(game, tournaFn, 5);
	}
	
	
	public boolean shouldMinimize() 
	{return false;}//we want to maximise scores

	
	public double calculateScore(final MLRegression contestant) 
	{
	  int noGames = totalGames; //x games each
	  long[] scores = new long[noGames];
	  
	 for (int i=0;i<noGames;i++) 
	   scores[i] = compete(contestant);
	  
	  return gameSum(scores) / noGames;
		
	}
	
	private long compete (final MLRegression contestant){
	  BasicNetwork opponent = pickRandom();
	   if (contestant != opponent)
	    return (Long)fitnessFn.invoke(tournaFn,
	                                  generatePlayer(GAME, contestant, 1), 
	                                  generatePlayer(GAME, opponent,  -1)); //the actual tournament
	   else 
	    return compete(contestant);//recurse once to play with someone else
	}
	
	private double gameSum(long[] scores)
	{
	  long overallScore = 0;
            for(int i=0;i<scores.length;i++)
	     overallScore += scores[i];
	  
	  return (double)overallScore;
	}
	
	//pick a random network from the population
	public BasicNetwork pickRandom()
	{ 
	  //Random generator = new Random();
	  //generate a random number
	  int rand = generator.nextInt(population.getPopulationSize() / 2); //get an organism from the best half of the population
		
	  Genome genome = population.get(rand);//pick a random organism from the population
	  BasicNetwork networkOpponent = (BasicNetwork) genome.getOrganism();//construct the network from the organism
	 
	  return networkOpponent;	
	}

	public void setPopulation(Population population){
		this.population = population;
	}
	
	public Population getPopulation(){
		return population;
	}
	
	private Object generatePlayer(Map<Keyword, Object> game, MLRegression brain, int direction){
	 return genPlayerFn.invoke(game, brain, direction);
	}

}
