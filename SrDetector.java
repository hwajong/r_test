package support_resistance;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

class Stock
{
	private Date date;
	private int price;
	
	public Stock(Date date, int price)
	{
		this.date = date;
		this.price = price;
	}
	
	public Date getDate()
	{
		return date;
	}
	public void setDate(Date date)
	{
		this.date = date;
	}
	public int getPrice()
	{
		return price;
	}
	public void setPrice(int price)
	{
		this.price = price;
	}
}

class Peak
{
	private Stock stock;
	private int similarPeakCount; // 비슷한 Peak 카운트
	
	public Date getDate()
	{
		return stock.getDate();
	}

	public int getPrice()
	{
		return stock.getPrice();
	}
	
	public Peak(Stock stock)
	{
		this.stock = stock;
		similarPeakCount = 0;
	}

	public int getSimilarPeakCount()
	{
		return similarPeakCount;
	}
	
	public void setSimilarPeakCount(int n)
	{
		similarPeakCount = n;
	}

	public void increseSimilarPeakCount()
	{
		this.similarPeakCount++;
	}
}

// Peak의 SimilarCount 를 기준으로 정렬하기위해
class PeakSimilarCountComparator implements Comparator<Peak> {
	@Override
	public int compare(Peak p1, Peak p2) {
		int c1 = p1.getSimilarPeakCount();
		int c2 = p2.getSimilarPeakCount();
		
		return c1 > c2 ? -1 : c1 < c2 ? 1:0;
	}
}

//Peak의 Price 를 기준으로 정렬하기위해
class PeakPriceComparator implements Comparator<Peak> {
	@Override
	public int compare(Peak p1, Peak p2) {
		int c1 = p1.getPrice();
		int c2 = p2.getPrice();
		
		return c1 > c2 ? -1 : c1 < c2 ? 1:0;
	}
}

// 지지선(Support), 저항선(Resistance) Detector
public class SrDetector
{
	private final int LAST_N_SAMPLE; // 몇개의 Sample data 사용?
	private final int WINDOW_SIZE_FOR_LOCAL_PEAK; // Peak 를 선택하기 위한 좌우 윈도우 크기
	private final double SIMILAR_PERCENT; // 비슷한 Peak 판단기준
	private final int N_MINOR; // Minor Peak 판단기준
	
	private SimpleDateFormat dateFmt = new SimpleDateFormat("yyyy년 MM월 dd일");
	
	public SrDetector(int LAST_N_SAMPLE, int WINDOW_SIZE_FOR_LOCAL_PEAK, double SIMILAR_PERCENT, int N_MINOR)
	{
		this.LAST_N_SAMPLE = LAST_N_SAMPLE;
		this.WINDOW_SIZE_FOR_LOCAL_PEAK = WINDOW_SIZE_FOR_LOCAL_PEAK;
		this.SIMILAR_PERCENT = SIMILAR_PERCENT;
		this.N_MINOR = N_MINOR;
	}
	
	private List<Stock> _readData(String fname) throws IOException, ParseException
	{
		File csv = new File(fname);
		BufferedReader br = new BufferedReader(new FileReader(csv));
		
		SimpleDateFormat fmt = new SimpleDateFormat("yyyy.MM.dd");
		String line = "";

		List<Stock> stockList = new ArrayList<Stock>();

        while ((line = br.readLine()) != null) 
        {
            // -1 옵션은 마지막 "," 이후 빈 공백도 읽기 위한 옵션
            String[] token = line.split(",", -1);
            
            Date date = fmt.parse(token[0]);
            int price = Integer.parseInt(token[1]);
            stockList.add(new Stock(date, price));
        }
        
        br.close();
        
        return stockList;
	}
	
	private void _debugStockList(List<Stock> stockList)
	{
		System.out.println("* STOCK LIST ------------------");
		for(Stock s : stockList)
		{
			System.out.println(String.format("%s : %d", dateFmt.format(s.getDate()), s.getPrice()));
		}
		
		System.out.println("* STOCK LIST SIZE : " + stockList.size());
		System.out.println("* -----------------------------");
	}
	
	private void _debugLocalPeakList(List<Peak> peakList, String desc)
	{
		System.out.println("* -----------------------------");
		System.out.println("* " + desc);
		System.out.println("* -----------------------------");
		for(Peak p : peakList)
		{
			System.out.println(String.format("%s : %d : %d", dateFmt.format(p.getDate()), p.getPrice(), p.getSimilarPeakCount()));
		}
		System.out.println("* PEAKS COUNT : " + peakList.size());
		System.out.println("* -----------------------------");
	}

	// index 위치의 값이 가장 큰값 ?
	private boolean isMaxInWindow(List<Stock> stockList, int index, int indexL, int indexR)
	{
		int max = stockList.get(indexL).getPrice();
		
		for(int i=indexL; i<=indexR; i++)
		{
			int p = stockList.get(i).getPrice();
			if(max < p) max = p;
		}
		
		if(stockList.get(index).getPrice() == max) return true;
		
		return false;
	}
	
	// index 위치의 값이 가장 작은값 ?
	private boolean isMinInWindow(List<Stock> stockList, int index, int indexL, int indexR)
	{
		int min = stockList.get(indexL).getPrice();
		
		for(int i=indexL; i<=indexR; i++)
		{
			int p = stockList.get(i).getPrice();
			if(min > p) min = p;
		}
		
		if(stockList.get(index).getPrice() == min) return true;
		
		return false;
	}
	
	private List<Peak> _findLocalPeaks(List<Stock> stockList, boolean isHighPeak)
	{
		List<Peak> peakList = new ArrayList<Peak>();
		
		int n = stockList.size();
		for(int i=0; i<n; i++)
		{
			int indexL = i - WINDOW_SIZE_FOR_LOCAL_PEAK;
			int indexR = i + WINDOW_SIZE_FOR_LOCAL_PEAK;
		    if(indexL < 0) indexL = 0;
		    if(indexR > n-1) indexR = n-1;
		    
		    if(isHighPeak)
		    {
			    if(isMaxInWindow(stockList, i, indexL, indexR))
			    {
			    	peakList.add(new Peak(stockList.get(i)));
			    }		    	
		    }
		    else // low peak
		    {
			    if(isMinInWindow(stockList, i, indexL, indexR))
			    {
			    	peakList.add(new Peak(stockList.get(i)));
			    }
		    }
		}
		
		return peakList;
	}
	
	private void _countSimilarPeak(List<Peak> peakList, double similarThreshold)
	{
		for(Peak p : peakList)
		{
			int low  = (int)(p.getPrice() * (1.0-similarThreshold));
			int high = (int)(p.getPrice() * (1.0+similarThreshold));
			
			for(Peak pp : peakList)
			{
				int c = pp.getPrice();
				if(c >= low && c <= high) p.increseSimilarPeakCount();
			}
		}
	}
	
	private void _cutSimilarPeak(List<Peak> peakList, double similarThreshold)
	{
		for(int i=0; i<peakList.size(); i++)
		{
			Peak p = peakList.get(i);
			
			if(p.getSimilarPeakCount() == 0) continue;
			
			int low  = (int)(p.getPrice() * (1.0-similarThreshold));
			int high = (int)(p.getPrice() * (1.0+similarThreshold));
			
			for(int j=0; j<peakList.size(); j++)
			{
				Peak pp = peakList.get(j);
				if(i != j && pp.getSimilarPeakCount() > 0)
				{
					int c = pp.getPrice();
					if(c >= low && c <= high) pp.setSimilarPeakCount(0);					
				}
			}
		}
		
		Iterator<Peak> i = peakList.iterator();
		while(i.hasNext())
		{
			Peak p = i.next();
			if(p.getSimilarPeakCount() == 0)
			{
				i.remove();
			}
		}
	}
	
	private void _cutMinorPeak(List<Peak> peakList)
	{
		Iterator<Peak> i = peakList.iterator();
		while(i.hasNext())
		{
			Peak p = i.next();
			if(p.getSimilarPeakCount() < N_MINOR)
			{
				i.remove();
			}
		}
	}
	
	public void detect(String fname) throws IOException, ParseException
	{
		List<Stock> stockList = _readData(fname);
		
		int size = stockList.size();
		if(size > LAST_N_SAMPLE)
		{
			stockList = stockList.subList(size - LAST_N_SAMPLE, stockList.size());
		}
		
		_debugStockList(stockList);
		
		List<Peak> peakList_high = _findLocalPeaks(stockList, true);
		Collections.sort(peakList_high, new PeakPriceComparator());
		_debugLocalPeakList(peakList_high, "로컬고점");
		
		List<Peak> peakList_low  = _findLocalPeaks(stockList, false);
		Collections.sort(peakList_low, new PeakPriceComparator());
		_debugLocalPeakList(peakList_low, "로컬저점");
		
		
		
		_countSimilarPeak(peakList_high, SIMILAR_PERCENT);
		Collections.sort(peakList_high, new PeakSimilarCountComparator());
		_debugLocalPeakList(peakList_high, "로컬고점 with Similar Counting");
		
		_countSimilarPeak(peakList_low, SIMILAR_PERCENT);
		Collections.sort(peakList_low, new PeakSimilarCountComparator());
		_debugLocalPeakList(peakList_low, "로컬저점 with Similar Counting");
		
		
		
		_cutSimilarPeak(peakList_high, SIMILAR_PERCENT);
		_debugLocalPeakList(peakList_high, "저항선");
		
		_cutSimilarPeak(peakList_low, SIMILAR_PERCENT);
		_debugLocalPeakList(peakList_low, "지지선");
		

		
		_cutMinorPeak(peakList_high);
		_debugLocalPeakList(peakList_high, "저항선 without Minor");
		
		_cutMinorPeak(peakList_low);
		_debugLocalPeakList(peakList_low, "지지선 without Minor");
	}
	
	public static void main(String args[]) throws IOException, ParseException
	{
		final int LAST_N_SAMPLE = 300;
		final int WINDOW_SIZE_FOR_LOCAL_PEAK = 3;
		final double SIMILAR_PERCENT = 0.05;
		final int N_MINOR = 5;
		
		SrDetector detector = new SrDetector(LAST_N_SAMPLE, WINDOW_SIZE_FOR_LOCAL_PEAK, SIMILAR_PERCENT, N_MINOR);
		detector.detect("하이소닉(106080).txt");
	}
}






















