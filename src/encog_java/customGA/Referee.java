package encog_java.customGA;

import java.util.Random;

import org.encog.neural.networks.BasicNetwork;
import org.encog.neural.networks.training.CalculateScore;
import org.encog.ml.genetic.genome.Genome;
import org.encog.ml.MLRegression;
import org.encog.ml.genetic.population.Population;

import clojure.lang.IFn;
import clojure.lang.RT;
import clojure.lang.Symbol;
import clojure.lang.Var;


/*This is the scoring object the genetic algorithm needs to tell the
 *network how well it is doing.
 * 
 * */
public final class Referee implements CalculateScore 
{
        
        private static IFn requireFn = RT.var("clojure.core", "require").fn();
         //{ requireFn.invoke(Symbol.intern("Clondie24.lib.core")); }
        private  IFn fitnessFn;  // RT.var("Clondie24.lib.core", "ga-fitness").fn(); //the fn we need
        private  IFn genPlayerFn; //RT.var("Clondie24.lib.core", "neural-player").fn(); 
	
	private Population population;
	
	public Referee(){ //prepare java-clojure interop
	   requireFn.invoke(Symbol.intern("Clondie24.lib.core"));
	   fitnessFn =   RT.var("Clondie24.lib.core", "ga-fitness").fn();//the fn we need
	   genPlayerFn = RT.var("Clondie24.lib.core", "neural-player").fn();
	}

	
	
	public boolean shouldMinimize() 
	{return false;}//we want to maximise scores

	
	public double calculateScore(final MLRegression contestant) 
	{
	  int noGames = 5; //5 games each
	  long[] scores = new long[noGames];
	  
	 for (int i=0;i<noGames;i++) 
	   scores[i] = compete(contestant);
	  
	  return fiveGameSum(scores);
		
	}
	
	private long compete (final MLRegression contestant){
	  BasicNetwork opponent = pickRandom();
	   if (!contestant.equals(opponent))
	    return (Long)fitnessFn.invoke(generatePlayer(contestant, 1), 
	                                  generatePlayer(opponent, -1));//the actual tournament
	   else 
	    return compete(contestant);//recurse once to play with someone else
	}
	
	private double fiveGameSum(long[] scores)
	{
	  long overallScore = 0;
            for(int i=0;i<scores.length;i++)
	     overallScore += scores[i];
	  
	  return (double)overallScore;
	}
	
	//pick a random network from the population
	public BasicNetwork pickRandom()
	{ 
	  Random generator = new Random();
	  //generate a random number
	  int rand = generator.nextInt(population.getPopulationSize()); 
		
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
	
	private Object generatePlayer(MLRegression brain, int direction){
	 return genPlayerFn.invoke(brain, direction);
	}

}
