using System;
using System.Collections;

namespace ProportionalRepresentation
{
    public class Party
    {
        public int number, votes, seats;

        public Party(int v)
        {
            number = list.Count + 1;
            votes = v;
            seats = 0;
            Party.total_votes += votes;
            list.Add(this);
        }

        public static ArrayList list = new ArrayList();
        public static int total_votes = 0;
        public static int quota = 0;
    }

    class LargestRemainderMethod
    {
        public class ScoreOperator : IComparer
        {
            int IComparer.Compare(object a, object b)
            {
                Party x = (Party) a;
                Party y = (Party) b;
                return (
                    x.votes - (x.seats * Party.quota) <
                    y.votes - (y.seats * Party.quota)
                ) ? 1 : -1;
            }
        }
        
        static void Main(string[] args)
        {
            new Party(400000);
            new Party(250000);
            new Party(100000);
            new Party(73000);
            new Party(5000);

            int seats, awarded = 0, awarding;
            seats = 5;
            Party.quota = (int) Math.Floor((double) Party.total_votes / seats);

            foreach (Party p in Party.list) 
            {
                awarding = (int) Math.Floor((double) p.votes / Party.quota);
                awarded += awarding;
                p.seats = awarding;
            }

            IComparer so = new ScoreOperator();
            Party.list.Sort(so);
            foreach (Party p in Party.list)
            {
                p.seats += 1;
                awarded += 1;
                if (awarded >= seats)
                    break;
            }

            foreach (Party p in Party.list)
                Console.WriteLine(Convert.ToString(p.number) + ", " + Convert.ToString(p.seats) + " seats");
        }
    }
}