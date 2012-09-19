package encog_java.customGA;

import java.util.Random;

import org.encog.neural.networks.BasicNetwork;
//import org.encog.neural.networks.training.CalculateScore;
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
public  class Referee implements CalculateScore 
{
        //prepare java-clojure interop
        private static IFn requireFn = RT.var("clojure.core", "require").fn();
        static {requireFn.invoke(Symbol.intern("Clondie24.games.chess"));}
        private static IFn fitnessFn = RT.var("Clondie24.games.chess", "ga-fitness").fn();//the fn we need
        private static IFn genPlayerFn = RT.var("Clondie24.games.chess", "generate-player").fn(); 
	
	private Population population;
	
	public Referee(){}

	
	@Override
	public boolean shouldMinimize() 
	{return false;}//we want to maximise scores

	@Override
	public double calculateScore(final MLRegression contestant) 
	{
	  int noGames = 5; //5 games each
	  double[] scores = new double[noGames];
	  
	 for (int i=0;i<noGames;i++) 
	  scores[i] = compete(contestant);
	  
	  return  fiveGameSum(scores);
		
	}
	
	private int compete (final MLRegression contestant){
	  BasicNetwork opponent = pickRandom();
	   if (!contestant.equals(opponent))
	    return (Integer)fitnessFn.invoke(generatePlayer(contestant, 1), 
	                                     generatePlayer(opponent, -1));//the actual tournament
	   else 
	    return compete(contestant);//recurse once to play with someone else
	}
	
	private double fiveGameSum(double[] scores)
	{
	  int overallScore = 0;
            for(int i=0;i<scores.length;i++)
	     overallScore += scores[i];
	  
	  return overallScore;
	}
	
	//pick a random network from the population
	public BasicNetwork pickRandom()
	{ 
	  Random generator = new Random();
	  //generate a random number
	  int rand = generator.nextInt(population. getPopulationSize()); 
		
	  Genome genome = population.get(rand);//pick a random organism from the population
	  BasicNetwork networkOpponent = (BasicNetwork) genome.getOrganism();//construct the network from the organism
	 
	  return networkOpponent;	
	}
	@Override
	public void setPopulation(Population population){
		this.population = population;
	}
	
	public Population getPopulation(){
		return this.population;
	}
	
	private Object generatePlayer(MLRegression brain, int direction){
	 return genPlayerFn.invoke(brain, direction);
	}

}
